//export = {};
var jsonString = "{\n    \"asd\": {\n        \"title\": \"Aproksymacje ASD\",\n        \"description\": \"Quiz sk\u0142adaj\u0105cy si\u0119 z pyta\u0144 pojawiaj\u0105cych si\u0119 na cz\u0119\u015Bci testowej egzaminu z przedmiotu Algorytmy i struktury danych (lub im podobnych), prowadzonego na wydziale MIMUW. Jako odpowied\u017A na pytania nale\u017Cy wpisa\u0107 liczb\u0119 ca\u0142kowit\u0105 b\u0119d\u0105c\u0105 przybli\u017Ceniem dok\u0142adnej odpowiedzi. W zale\u017Cno\u015Bci od tego jak bardzo wpisany wynik odbiega od przedzia\u0142u uznawanego za poprawny dolicza si\u0119 odpowiedni\u0105 kar\u0119.\",\n        \"example\": \"Ile razy zd\u0105\u017Cy zmutowa\u0107 wirus z Covid-19 w przeci\u0105gu roku?\",\n        \"example_answer\": 25,\n        \"example_punishments\": \"Poprawny przedzia\u0142: 20-35 Kara +10s: [10-20) U (35-45] Kara +20s: [0; 10) U (45; oo]\",\n        \"type\": 1,\n        \"questions_no\": 4,\n        \"punishments\": [0, 10, 20],\n\n        \"questions\": [\n            {\n                \"question\": \"Wybierasz losow\u0105 stron\u0119 z ksi\u0105\u017Cki \u201CWprowadzenie do algorytm\u00F3w\u201D. Jakie jest prawdopodobie\u0144stwo tego, \u017Ce wybrana przez Ciebie strona jest liczb\u0105 pierwsz\u0105? Odpowied\u017A wyra\u017A w procentach zaokr\u0105glaj\u0105c do cyfr jedno\u015Bci, pomijaj\u0105c znak '%' na ko\u0144cu.\",\n                \"punishments\": [[1, 10], [25, 50]],\n                \"correct_answer\": [10, 25]\n            },\n\n            {\n                \"question\": \"Ka\u017Cdy ze 100 student\u00F3w wybiera losowo i niezale\u017Cnie od pozosta\u0142ych jedn\u0105 stron\u0119 z polskiego wydania ksi\u0105\u017Cki Wprowadzenie do algorytm\u00F3w Cormena, Leisersona, Rivesta i Steina. Wska\u017C przedzia\u0142, w kt\u00F3rym znajduje si\u0119 prawdopodobie\u0144stwo zdarzenia, \u017Ce pewnych dw\u00F3ch student\u00F3w wybra\u0142o t\u0119 sam\u0105 stron\u0119. Odpowied\u017A wyra\u017A w procentach zaokr\u0105glaj\u0105c do cyfr jedno\u015Bci, pomijaj\u0105c znak '%' na ko\u0144cu.\",\n                \"punishments\": [[75, 89], [25, 75]],\n                \"correct_answer\": [89, 100]\n            },\n\n            {\n                \"question\": \"Jaka warto\u015B\u0107 pieni\u0119\u017Cna w monetach jednoz\u0142otowych zmie\u015Bci si\u0119 w standardowej wielko\u015Bci wiadrze?\",\n                \"punishments\": [[1000, 5000], [10000, 50000]],\n                \"correct_answer\": [5000, 10000]\n            },\n\n            {\n                \"question\": \"Ile chococino z automatu 'Cafe+co' na wydziale MIMUW trzeba wypi\u0107 \u017Ceby spe\u0142ni\u0107 dzienne zapotrzebowanie na cukier wskazywane przez GDA?\",\n                \"punishments\": [[4, 6], [9, 11]],\n                \"correct_answer\": [6, 9]\n            }\n        ]\n    },\n\n    \"basic\": {\n        \"title\": \"Dodawanie\",\n        \"description\": \"Klasyczny quiz algebraiczny sprawdzaj\u0105cy umiej\u0119tno\u015B\u0107 dodawania. Jako odpowied\u017A nale\u017Cy poda\u0107 liczb\u0119 ca\u0142kowit\u0105, przy podaniu b\u0142\u0119dnej odpowiedzi naliczana jest 6 sekundowa kara.\",\n        \"example\": \"2 + 2 = ?\",\n        \"example_answer\": 4,\n        \"example_punishments\": \"Kara za b\u0142\u0119dn\u0105 odpowied\u017A: +6s.\",\n        \"type\": 2,\n        \"questions_no\": 6,\n        \"punishments\": [0, 6],\n\n        \"questions\": [\n            {\n                \"question\": \"36 + 11 = ?\",\n                \"punishments\": [],\n                \"correct_answer\": [47]\n            },\n\n            {\n                \"question\": \"46 + 15 = ?\",\n                \"punishments\": [],\n                \"correct_answer\": [61]\n            },\n\n            {\n                \"question\": \"45 + 17 = ?\",\n                \"punishments\": [],\n                \"correct_answer\": [62]\n            },\n\n            {\n                \"question\": \"67 + 8 + 82 = ?\",\n                \"punishments\": [],\n                \"correct_answer\": [157]\n            },\n\n            {\n                \"question\": \"27 + 7 + 23 = ?\",\n                \"punishments\": [],\n                \"correct_answer\": [57]\n            },\n\n            {\n                \"question\": \"93 + 94 + 73 = ?\",\n                \"punishments\": [],\n                \"correct_answer\": [260]\n            }\n        ]\n    }\n}";
var quizzes = JSON.parse(jsonString);
/* HTML ELEMENTS USED IN SCRIPT */
var htmlTitle = document.getElementById("result_title");
var htmlResultTabBody = document.getElementById("result_tab_body");
var htmlFullResult = document.getElementById("full_result");
var htmlResultTabSum = document.getElementById("result_tab_sum");
var saveButton = document.getElementById("save_result");
var saveStatsButton = document.getElementById("save_stats");
var timers = JSON.parse(sessionStorage.getItem("timers"));
var userAnswers = JSON.parse(sessionStorage.getItem("answers"));
var questionsNumber = 0;
var quizTitle = "";
var questions = [];
var quizType = 0;
var quizPunishments = [];
var punishmentSum = 0;
var timeSpent = 0;
var quizId = sessionStorage.getItem("quiz");
/**
 * Function setting 'quiz-describing' variables according to value set in 'sessionStorage'.
 */
function setQuizType() {
    if (quizId === "asd") {
        questions = quizzes.asd.questions;
        questionsNumber = quizzes.asd.questions_no;
        quizTitle = quizzes.asd.title;
        quizType = quizzes.asd.type;
        quizPunishments = quizzes.asd.punishments;
    }
    else if (quizId === "basic") {
        questions = quizzes.basic.questions;
        questionsNumber = quizzes.basic.questions_no;
        quizTitle = quizzes.basic.title;
        quizType = quizzes.basic.type;
        quizPunishments = quizzes.basic.punishments;
    }
}
/**
 * Function telling how big punishment for user answer should be given.
 * @param type Type of quiz which tells how function should be used.
 * @param k    Index of question (and answer to it) to be checked.
 */
function setPunishment(type, k) {
    if (type === 1) {
        if ((userAnswers[k] >= questions[k].punishments[0][0] && userAnswers[k] <= questions[k].punishments[0][1]) || (userAnswers[k] >= questions[k].punishments[1][0] && userAnswers[k] <= questions[k].punishments[1][1])) {
            return 1;
        }
        else if (userAnswers[k] > questions[k].correct_answer[0] && userAnswers[k] < questions[k].correct_answer[1]) {
            return 0;
        }
        else {
            return 2;
        }
    }
    else if (type === 2) {
        if (+userAnswers[k] !== questions[k].correct_answer[0]) {
            return 1;
        }
        else {
            return 0;
        }
    }
}
/**
 *
 * @param node Node to which append the table cell.
 * @param type Type of quiz which tells how function should be used.
 * @param i    Index of question corresponding to this table cell.
 */
function setPunishmentTabCell(node, type, i) {
    var elem = document.createElement("td");
    var x = setPunishment(type, i);
    punishmentSum += quizPunishments[x];
    if (type === 1) {
        if (x === 0) {
            elem.textContent = "Poprawna odpowied\u017A: +" + quizPunishments[x] + "s";
            elem.setAttribute("style", "color: green;");
        }
        else if (x === 1) {
            elem.textContent = "B\u0142\u0119dna odpowied\u017A: +" + quizPunishments[x] + "s";
            elem.setAttribute("style", "color: #e2df06;");
        }
        else {
            elem.textContent = "B\u0142\u0119dna odpowied\u017A: +" + quizPunishments[x] + "s";
            elem.setAttribute("style", "color: red;");
        }
    }
    else if (type === 2) {
        if (x === 0) {
            elem.textContent = "Poprawna odpowied\u017A: +" + quizPunishments[x] + "s";
            elem.setAttribute("style", "color: green;");
        }
        else {
            elem.textContent = "B\u0142\u0119dna odpowied\u017A: +" + quizPunishments[x] + "s";
            elem.setAttribute("style", "color: red;");
        }
    }
    node.appendChild(elem);
}
function setTable(type) {
    for (var i = 0; i < questionsNumber; i++) {
        var node = document.createElement("tr");
        var elem = document.createElement("td");
        elem.textContent = "Zadanie " + (i + 1);
        node.appendChild(elem);
        elem = document.createElement("td");
        elem.textContent = userAnswers[i].toString();
        node.appendChild(elem);
        setPunishmentTabCell(node, type, i);
        htmlResultTabBody.appendChild(node);
    }
}
function sumTimers() {
    for (var _i = 0, timers_1 = timers; _i < timers_1.length; _i++) {
        var timer = timers_1[_i];
        timeSpent += timer;
    }
}
function saveResult() {
    var x;
    if (quizId === "asd") {
        x = 0;
    }
    else if (quizId === "basic") {
        x = 1;
    }
    var results = JSON.parse(localStorage.getItem("results"));
    results[x].push(+(((timeSpent / 1000) + punishmentSum).toFixed(2)));
    localStorage.setItem("results", JSON.stringify(results));
    window.location.href = "main-page.html";
}
function saveResultWithStats() {
    var x;
    if (quizId === "asd") {
        x = 0;
    }
    else if (quizId === "basic") {
        x = 1;
    }
    var results = JSON.parse(localStorage.getItem("results"));
    results[x].push(+(((timeSpent / 1000) + punishmentSum).toFixed(2)));
    localStorage.setItem("results", JSON.stringify(results));
    var stats = JSON.parse(localStorage.getItem("stats"));
    stats[x].push(timers);
    localStorage.setItem("stats", JSON.stringify(stats));
    window.location.href = "main-page.html";
}
/* MAIN EXECUTION OF SCRIPT */
setQuizType();
htmlTitle.textContent = quizTitle;
setTable(quizType);
htmlResultTabSum.textContent = "+" + punishmentSum;
if (punishmentSum > 0) {
    htmlResultTabSum.setAttribute("style", "color: red;");
}
else {
    htmlResultTabSum.setAttribute("style", "color: green;");
}
sumTimers();
htmlFullResult.textContent = ((timeSpent / 1000) + punishmentSum).toFixed(2) + "s";
saveButton.setAttribute("onclick", "saveResult()");
saveStatsButton.setAttribute("onclick", "saveResultWithStats()");
