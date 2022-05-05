const express = require('express');
const router = express.Router();
const path = require('path');
const app = express()
var sqlite3 = require('sqlite3').verbose();

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

/* Get main page. */
router.get('/', (req, res) => {
    //console.log(app);
    res.sendFile(path.join(__dirname, '../public/main-page.html'));
});

router.get('/login', function(req, res) {
    forbidLogged(req, res);
    
    res.sendFile(path.join(__dirname, '../public/login.html'));
});

router.post('/login', function(req, res, next) {
    req.db.get('SELECT login FROM users WHERE login = ? AND password = ?', [req.body.login, req.body.password],
    function(err, row) {
        checkInternalError(err, req, res);

        if (row != undefined) {
            req.session.login = row.login;
            res.redirect('/');
        }
        else {
            res.redirect('/login');
        }
    });
});

router.get('/logout-all', function (req, res, next) {
    forbidUnauthorized(req, res);

    const sessionsDb = new sqlite3.Database(path.join(__dirname, '../databases/sessions.db'));

    sessionsDb.all('SELECT * FROM sessions;', function (err, sessions) {
        sessionsDb.serialize(function () {
            for (let session of sessions) {
                //console.log(session);
                let sess = JSON.parse(session.sess);
                //console.log(sess);
                if (sess.login == req.session.login) {
                    delete(sess.login);
                    sessionsDb.run('UPDATE sessions SET sess = ? WHERE sid = ?', [JSON.stringify(sess), session.sid]);
                }
            }
        });

        res.redirect('/');
    });
});

router.get('/logout', function(req, res, next) {
    forbidUnauthorized(req, res);

    delete(req.session.login);
    res.redirect('/');
});

router.get('/change-pwd', function(req, res, next) {
    forbidUnauthorized(req, res);

    res.sendFile(path.join(__dirname, '../public/change-pwd.html'));
});

router.post('/change-pwd', function(req, res, next) {
    forbidUnauthorized(req, res);

    req.db.run('UPDATE users SET password = ? WHERE login = ?', [req.body.newPwd, req.session.login], 
    function (err) {
        checkInternalError(err, req, res);
        
        const sessionsDb = new sqlite3.Database(path.join(__dirname, '../databases/sessions.db'));

        sessionsDb.all('SELECT * FROM sessions;', function (err, sessions) {
            sessionsDb.serialize(function () {
                for (let session of sessions) {
                    //console.log(session);
                    let sess = JSON.parse(session.sess);
                    //console.log(sess);
                    if (sess.login == req.session.login) {
                        delete(sess.login);
                        sessionsDb.run('UPDATE sessions SET sess = ? WHERE sid = ?', [JSON.stringify(sess), session.sid]);
                    }
                }
            });
    
            res.redirect('/');
        });
    });
});

router.get('/stats', function(req, res, next) {
    forbidUnauthorized(req, res);

    res.sendFile(path.join(__dirname, '../public/stats.html'));
});

module.exports = router;
