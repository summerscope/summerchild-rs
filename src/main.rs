use serde::{Deserialize, Deserializer};
use serde::de::{self, Visitor};

use std::io;
use std::fmt;
use std::error::Error;
use std::fs::File;
use std::io::BufReader;
use std::io::Write;
use std::path::Path;
use std::collections::BTreeMap as Map;

type Score = f64;

#[derive(Debug)]
enum ScoreOrEmpty {
	Value(Score),
	Empty
}

#[derive(Deserialize, Debug)]
struct Answer {
	text: String,
	multiplier: Option<ScoreOrEmpty>,
	score: Option<ScoreOrEmpty>,
	recommendation: Option<String>,
	nextq: String
}

#[derive(Deserialize, Debug)]
struct Question {
	id: String,
	text: String,
	answers: Map<String, Answer>
}

#[derive(Debug)]
struct ScoreRange {
	low: Score,
	high: Score,
	text: String
}

#[derive(Deserialize, Debug)]
struct QuizResult {
	range: ScoreRange,
	title: String,
	text: String
}

#[derive(Deserialize, Debug)]
struct QuizResults {
	#[allow(dead_code)]
	id: String,
	results: Map<String, QuizResult>
}

#[derive(Deserialize, Debug)]
#[serde(untagged)]
enum QuizItem {
	Question(Question),
	Results(QuizResults)
}

type QuizData = Vec<QuizItem>;

#[derive(Debug)]
struct State {
	multiplier: Score,
	score: Score,
	currentq: String,
	recommendations: Vec<String>,
	history: Vec<(String, String)>,
	finished: bool
}

impl State {
	fn new() -> State {
		State {
			multiplier: 1.0,
			score: 0.0,
			currentq: String::from("Q1"),
			recommendations: vec![],
			history: vec![],
			finished: false
		}
	}
}

#[derive(Debug)]
enum AskError {
	InvalidAnswer(String),
	IOError(io::Error),
}

impl From<io::Error> for AskError {
	fn from(item: io::Error) -> Self {
		AskError::IOError(item)
	}
}

#[derive(Debug)]
enum QuizError {
	IOError(io::Error),
	JSONError(serde_json::Error),
	ResultsSectionMissing,
}

impl fmt::Display for QuizError {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match self {
			QuizError::JSONError(err) => write!(f, "{}", err),
			QuizError::IOError(err)   => write!(f, "{}", err),
			QuizError::ResultsSectionMissing => write!(f, "quiz results section not found")
		}
	}
}

impl From<io::Error> for QuizError {
	fn from(item: io::Error) -> Self {
		QuizError::IOError(item)
	}
}

impl From<serde_json::Error> for QuizError {
	fn from(item: serde_json::Error) -> Self {
		QuizError::JSONError(item)
	}
}

impl Error for QuizError { }

struct ScoreRangeVisitor;

impl<'de> Visitor<'de> for ScoreRangeVisitor {
	type Value = ScoreRange;

	fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
		formatter.write_str("score range of format \"2-31\"")
	}

	fn visit_str<E: de::Error>(self, v: &str) -> Result<Self::Value, E>
	{
		let r = parse_range(v);

		match r {
			Some(r) => Ok(r),
			None    => Err(de::Error::custom("invalid score range"))
		}
	}
}

impl<'de> Deserialize<'de> for ScoreRange {
	fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
	where
		D: Deserializer<'de>,
	{
		deserializer.deserialize_str(ScoreRangeVisitor)
	}
}

struct ScoreOrEmptyVisitor;

impl<'de> Visitor<'de> for ScoreOrEmptyVisitor {
	type Value = ScoreOrEmpty;

	fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
		formatter.write_str("score or empty string")
	}

	fn visit_str<E: de::Error>(self, v: &str) -> Result<Self::Value, E>
	{
		if v.is_empty() {
			Ok(ScoreOrEmpty::Empty)
		} else {
			Err(de::Error::custom("non-empty string where score was expected"))
		}
	}

	fn visit_f64<E: de::Error>(self, v: f64) -> Result<Self::Value, E> { Ok(ScoreOrEmpty::Value(v)) }
	fn visit_u64<E: de::Error>(self, v: u64) -> Result<Self::Value, E> { Ok(ScoreOrEmpty::Value(v as f64)) }
	fn visit_i64<E: de::Error>(self, v: i64) -> Result<Self::Value, E> { Ok(ScoreOrEmpty::Value(v as f64)) }
}

impl<'de> Deserialize<'de> for ScoreOrEmpty {
	fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
	where
		D: Deserializer<'de>,
	{
		deserializer.deserialize_any(ScoreOrEmptyVisitor)
	}
}

fn read_quiz_json<P: AsRef<Path>>(path: P) -> Result<QuizData, QuizError> {
	let file = File::open(path)?;
	let reader = BufReader::new(file);

	let quiz: QuizData = serde_json::from_reader(reader)?;

	Ok(quiz)
}

fn hash_question_ids(qs: &Vec<Question>) -> Map<&String, &Question> {
	qs.iter().map(|q| (&q.id, q)).collect()
}

fn input_line() -> io::Result<String> {
	let mut buf = String::new();
	io::stdin().read_line(&mut buf)?;
	return Ok(buf);
}

fn print_prompt() -> io::Result<()> {
	print!("> ");
	io::stdout().flush()
}

fn try_ask_question(question: &Question) -> Result<(String, &Answer), AskError> {
	println!("{}", question.text);

	for (key, answer) in &question.answers {
		println!("{}: {}", key, answer.text);
	}

	print_prompt()?;
	let key = input_line()?.trim().to_string();

	let chosen_answer = question.answers.iter().find_map(|(k, v)| {
		if k.to_lowercase() == key.to_lowercase() {
			Some(v)
		} else {
			None
		}
	});

	if chosen_answer.is_none() {
		println!("invalid choice: {}", key);
	}

	match chosen_answer {
		Some(answer) => Ok((key, answer)),
		None         => Err(AskError::InvalidAnswer(key))
	}
}

fn ask_question(question: &Question) -> io::Result<(String, &Answer)> {
	let mut answer;

	loop {
		answer = try_ask_question(question);

		match answer {
			Ok(answer)                      => return Ok(answer),
			Err(AskError::IOError(err))     => return Err(err),
			Err(AskError::InvalidAnswer(_)) => continue,
		}
	};
}

fn update_state(state: State, question: &Question, answer: &Answer, answer_key: String) -> State {
	fn get_score(o: &Option<ScoreOrEmpty>, default: Score) -> Score {
		use ScoreOrEmpty::*;

		match o {
			Some(Value(v)) => *v,
			Some(Empty)    =>  default,
			None           =>  default
		}
	}

	let mut next_state = State {
		multiplier: get_score(&answer.multiplier, state.multiplier),
		score: state.score + get_score(&answer.score, 0.0),
		currentq: answer.nextq.clone(),
		finished: answer.nextq.is_empty(),
		..state
	};

	if let Some(rec) = answer.recommendation.as_ref() {
		if !rec.is_empty() {
			next_state.recommendations.push(rec.clone());
		}
	}

	next_state.history.push((question.id.clone(), answer_key));

	return next_state;
}

fn parse_range(range_str: &str) -> Option<ScoreRange> {
	let mut split = range_str.split("-");
	let low:  Score  = split.next()?.parse().ok()?;
	let high: Score  = split.next()?.parse().ok()?;
	let text: String = range_str.to_string();

	Some(ScoreRange {low, high, text})
}

fn print_summary(state: &State, results_section: &QuizResults) {
	println!("---------- Results ----------");
	println!("Your score {}/99", state.score);

	for res in results_section.results.values() {
		if state.score >= res.range.low && state.score <= res.range.high {
			println!("{}: {}\n\n{}", res.range.text, res.title, res.text);
		}
	}

	println!("Recommendations:");
	for rec in state.recommendations.iter() {
		println!("- {}", rec);
	}
}

fn load_quiz_data() -> Result<(Vec<Question>, QuizResults), QuizError> {
	let quiz_data = read_quiz_json("questions.json")?;

	let mut questions: Vec<Question> = vec![];
	let mut results_section: Option<QuizResults> = None;

	for item in quiz_data {
		match item {
			QuizItem::Question(q) => questions.push(q),
			QuizItem::Results(r)  => results_section = Some(r)
		}
	}

	match results_section {
		Some(results_section) => return Ok((questions, results_section)),
		None                  => return Err(QuizError::ResultsSectionMissing)
	}
}

fn run_quiz() -> Result<(), QuizError> {
	let mut state = State::new();
	// state = State { currentq: String::from("Q27"), ..state };

	let (questions, results_section) = load_quiz_data()?;
	let questions_by_id = hash_question_ids(&questions);

	while !state.finished {
		let question = questions_by_id[&state.currentq];
		let (answer_key, answer) = ask_question(question)?;
		state = update_state(state, question, answer, answer_key);
	}

	print_summary(&state, &results_section);

	Ok(())
}

fn main() {
	let res = run_quiz();

	if let Err(err) = res {
		eprintln!("error: {}", err);
		std::process::exit(1);
	}
}
