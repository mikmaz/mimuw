const express = require('express');
const router = express.Router();
const path = require('path');

router.get('/solved', function(req, res, next) {
    res.sendFile(path.join(__dirname, '../public/solved-err.html'));
});

module.exports = router;
