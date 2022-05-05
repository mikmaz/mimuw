const express = require('express');
const router = express.Router();
const path = require('path');

/* FUNCTIONS FOR THROWING ERRORS */

const forbidUnauthorized = function (req, res) {
  if (!req.session.login) {
      res.statusCode = 401;
      throw new Error("Unauthorized");
  }
};

const forbidLogged = function (req, res) {
  if (req.session.login) {
      res.statusCode = 403;
      throw new Error("Forbidden");
  }
};

const checkInternalError = function (err, req, res) {
  if (err) {
      res.statusCode = 500;
      throw new Error("Internal Server Error");
  }
}

/* Get information whether user is logged or not. */
router.get('/user-info', function(req, res) {
  res.json({login: req.session.login});
});

/* Get list of all quizzes in database */
router.get('/quiz-list', function(req, res) {
  req.db.all("SELECT name, title, description, example, example_answer, example_punishments FROM quizzes;", 
  function(err, quizzes) {
    checkInternalError(err, req, res);
      
    res.json(quizzes);
  });
});

router.get('/quiz', function(req, res, next) {
  req.db.get("SELECT questions_no, title, description FROM quizzes WHERE name = ?", [req.session.quiz], 
  function(err, quizInfo) {
    checkInternalError(err, req, res);

    req.db.all("SELECT * FROM questions WHERE questions.quiz_name = ? ORDER BY question_no ASC", [req.session.quiz], 
    function(err, questions) {
      checkInternalError(err, req, res);

      quizInfo.questions = questions;
      res.json(quizInfo);
    });
  });
});

/* User sends quiz answers. */
router.post('/result', function(req, res) {
  let time = Math.floor((new Date().getTime() - req.session.quiz_start) / 1000); // how long it took user to solve quiz
  let fullResult = time; // variable storing full score of quiz (time spent + punishment for wrong answer)

  req.db.get("SELECT correct_answers, punishment FROM quizzes WHERE name = ?", [req.session.quiz], function (err, row) {
    checkInternalError(err, req, res);

    let correctAnswers = JSON.parse(row.correct_answers);
    for (let i = 0; i < correctAnswers.length; i++) {
      if (correctAnswers[i] != req.body.answers[i]) {
        fullResult += row.punishment;
      }
    }

    req.db.run("INSERT INTO results (user_login, quiz_name, time_spent, answers, stats, full_res) " +
    "VALUES (?, ?, ?, ?, ?, ?)", [req.session.login, req.session.quiz, time, JSON.stringify(req.body.answers), 
    JSON.stringify(req.body.stats), fullResult], function (err) {
      checkInternalError(err, req, res);

      delete(req.session.quiz_id);
      delete(req.session.quiz_name);
  
      res.json({ message: "OK" });
    });
  })
});

/* Get list of quizzes solved by logged user. */
router.get('/usr-solved-quizzes', function(req, res) {
  req.db.all("SELECT quiz_name, title FROM results JOIN quizzes ON quizzes.name = results.quiz_name " +
  "WHERE user_login = ?", [req.session.login], function(err, userQuizzes) {
    checkInternalError(err, req, res);

    res.json(userQuizzes);
  });
})

/* Get stats of quiz (@param quizName) for logged user. */
router.get('/stats/:quizName', function(req, res) {
  forbidUnauthorized(req, res);

  var resJson = <any>{};
  resJson.user = req.session.login;

  req.db.all("SELECT * FROM results WHERE quiz_name = ?", [req.params.quizName], function (err, quizResults) {
    checkInternalError(err, req, res);

    var averages = [];
    var questions_no = JSON.parse(quizResults[0].answers).length;

    for(let i = 0; i < questions_no; i++) {
      averages.push(0);
    }

    for(let result of quizResults) {
      if (result.user_login == req.session.login) {
        resJson.usr_answers = JSON.parse(result.answers);
        resJson.full_res = result.full_res;
      }

      let timers = JSON.parse(result.stats);
      for(let i = 0; i < questions_no; i++) {
        averages[i] += result.time_spent * (timers[i] / 100);
      }
    }

    for(let i = 0; i < questions_no; i++) {
      averages[i] = Math.floor(averages[i] / quizResults.length);
    }

    resJson.averages = averages;

    req.db.all("SELECT user_login, full_res FROM results WHERE quiz_name = ? ORDER BY full_res LIMIT 5;", 
    [req.params.quizName], function(err, topUsers) {
      checkInternalError(err, req, res);

      resJson.topUsers = topUsers;

      req.db.get("SELECT correct_answers, punishment FROM quizzes WHERE name = ?", [req.params.quizName], 
      function(err, selectedQuiz) {
        checkInternalError(err, req, res);

        resJson.correct_answers = JSON.parse(selectedQuiz.correct_answers);
        resJson.punishment = selectedQuiz.punishment;

        res.json(resJson);
      });
    })
  })
});

module.exports = router;
