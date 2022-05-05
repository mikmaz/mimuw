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

const checkInternalError = function (err, req, res) {
    if (err) {
        res.statusCode = 500;
        throw new Error("Internal Server Error");
    }
};

router.get('/:quiz_name', function(req, res, next) {
    forbidUnauthorized(req, res);

    req.db.get("SELECT * FROM results WHERE user_login = ? AND quiz_name = ?",[req.session.login, req.params.quiz_name],
    function(err, row) {
        checkInternalError(err, req, res);

        if (row != undefined) {
            res.redirect('/error/solved');
        }
        else {
            req.session.quiz = req.params.quiz_name;
            req.session.quiz_start = new Date().getTime();
            res.sendFile(path.join(__dirname, '../public/quiz.html'));
        }
    }); 
});

module.exports = router;
