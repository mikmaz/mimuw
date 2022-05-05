const path = require('path');
const fs = require('fs');
const sqlite3 = require('sqlite3').verbose();
const quizzes = require(path.join(__dirname, '../jsons/quiz_info.json')); // get json storing information about quizzes

/* Create database file or empty it if already exists. */
fs.writeFile(path.join(__dirname, '../databases/quizgebra.db'), '', function (err) {
  if (err) return console.log(err);
});

var db = new sqlite3.Database(path.join(__dirname, '../databases/quizgebra.db'));

/* Initialize database */
db.serialize(function() {
  db.run("CREATE TABLE users (id INTEGER PRIMARY KEY AUTOINCREMENT, login VARCHAR, password VARCHAR)");
  db.run("INSERT INTO users (login, password) VALUES ('user1', 'user1')");
  db.run("INSERT INTO users (login, password) VALUES ('user2', 'user2')");

  db.run("CREATE TABLE quizzes (id INTEGER PRIMARY KEY AUTOINCREMENT, name VARCHAR, title VARCHAR, " + 
    "description VARCHAR, example VARCHAR, example_answer VARCHAR, example_punishments VARCHAR, " + 
    "questions_no INTEGER, correct_answers VARCHAR, punishment INTEGER);");

  for (let quiz of quizzes) {
    db.run("INSERT INTO quizzes (name, title, description, example, example_answer, example_punishments, " +
      "questions_no, correct_answers, punishment) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?);", [quiz.name, quiz.title,
      quiz.description, quiz.example, quiz.example_answer, quiz.example_punishments, quiz.questions_no, 
      JSON.stringify(quiz.correct_answers), quiz.punishment]);
  }

  db.run("CREATE TABLE questions (id INTEGER PRIMARY KEY AUTOINCREMENT, quiz_name VARCHAR, question VARCHAR, " +
    "question_no INTEGER);");

  for (let quiz of quizzes) {
    for (let question of quiz.questions) {
      db.run("INSERT INTO questions (quiz_name, question, question_no) VALUES (?, ?, ?);", [quiz.name, 
        question.question, question.question_no]);
    }
  }

  db.run("CREATE TABLE results (id INTEGER PRIMARY KEY AUTOINCREMENT, user_login VARCHAR, quiz_name VARCHAR, " +
    "time_spent INTEGER, answers VARCHAR, stats VARCHAR, full_res INTEGER);");
});
