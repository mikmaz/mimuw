var express = require('express');
var path = require('path');
var cookieParser = require('cookie-parser');
var session = require('express-session');
var SQLiteStore = require('connect-sqlite3')(session);

var app = express();
var sqlite3 = require('sqlite3').verbose();
var db = new sqlite3.Database(path.join(__dirname, 'databases/quizgebra.db'));

app.use(express.json());
app.use(express.urlencoded({ extended: false }));
app.use(express.static('public'));

app.use(cookieParser(app.get('sAn7baRbhx4')));
app.use(session({
    secret: 'sAn7baRbhx4',
    cookie: { maxAge: 15 * 60 * 1000 * 4 }, // 60 min
    resave: false,
    saveUninitialized: true,
    store: new SQLiteStore({db: 'sessions.db', table: 'sessions', dir: './databases'}),
  })
);

app.logout = true;

var jsonsRouter = require('./routes/jsons.js');
var indexRouter = require('./routes/index.js');
var quizRouter  = require('./routes/quiz.js');
var errorRouter = require('./routes/error.js')

app.use(function(req, res, next) {
  req.db = db;
  next();
})

app.use('/json', jsonsRouter);
app.use('/quiz', quizRouter);
app.use('/error', errorRouter);
app.use('/', indexRouter);

app.use(function (req, res, next) {
  res.statusCode = 404;
  throw new Error("Not found");
})

app.use(function (err, req, res, next) {
  res.locals.message = err.message;
  //res.locals.error = req.app.get("env") === "development" ? err : {};

  res.json({message: err.message})
});

module.exports = app;