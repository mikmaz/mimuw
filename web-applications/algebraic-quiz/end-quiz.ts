//export = {};

let jsonString: string = `{
    "asd": {
        "title": "Aproksymacje ASD",
        "description": "Quiz składający się z pytań pojawiających się na części testowej egzaminu z przedmiotu Algorytmy i struktury danych (lub im podobnych), prowadzonego na wydziale MIMUW. Jako odpowiedź na pytania należy wpisać liczbę całkowitą będącą przybliżeniem dokładnej odpowiedzi. W zależności od tego jak bardzo wpisany wynik odbiega od przedziału uznawanego za poprawny dolicza się odpowiednią karę.",
        "example": "Ile razy zdąży zmutować wirus z Covid-19 w przeciągu roku?",
        "example_answer": 25,
        "example_punishments": "Poprawny przedział: 20-35 Kara +10s: [10-20) U (35-45] Kara +20s: [0; 10) U (45; oo]",
        "type": 1,
        "questions_no": 4,
        "punishments": [0, 10, 20],

        "questions": [
            {
                "question": "Wybierasz losową stronę z książki “Wprowadzenie do algorytmów”. Jakie jest prawdopodobieństwo tego, że wybrana przez Ciebie strona jest liczbą pierwszą? Odpowiedź wyraź w procentach zaokrąglając do cyfr jedności, pomijając znak '%' na końcu.",
                "punishments": [[1, 10], [25, 50]],
                "correct_answer": [10, 25]
            },

            {
                "question": "Każdy ze 100 studentów wybiera losowo i niezależnie od pozostałych jedną stronę z polskiego wydania książki Wprowadzenie do algorytmów Cormena, Leisersona, Rivesta i Steina. Wskaż przedział, w którym znajduje się prawdopodobieństwo zdarzenia, że pewnych dwóch studentów wybrało tę samą stronę. Odpowiedź wyraź w procentach zaokrąglając do cyfr jedności, pomijając znak '%' na końcu.",
                "punishments": [[75, 89], [25, 75]],
                "correct_answer": [89, 100]
            },

            {
                "question": "Jaka wartość pieniężna w monetach jednozłotowych zmieści się w standardowej wielkości wiadrze?",
                "punishments": [[1000, 5000], [10000, 50000]],
                "correct_answer": [5000, 10000]
            },

            {
                "question": "Ile chococino z automatu 'Cafe+co' na wydziale MIMUW trzeba wypić żeby spełnić dzienne zapotrzebowanie na cukier wskazywane przez GDA?",
                "punishments": [[4, 6], [9, 11]],
                "correct_answer": [6, 9]
            }
        ]
    },

    "basic": {
        "title": "Dodawanie",
        "description": "Klasyczny quiz algebraiczny sprawdzający umiejętność dodawania. Jako odpowiedź należy podać liczbę całkowitą, przy podaniu błędnej odpowiedzi naliczana jest 6 sekundowa kara.",
        "example": "2 + 2 = ?",
        "example_answer": 4,
        "example_punishments": "Kara za błędną odpowiedź: +6s.",
        "type": 2,
        "questions_no": 6,
        "punishments": [0, 6],

        "questions": [
            {
                "question": "36 + 11 = ?",
                "punishments": [],
                "correct_answer": [47]
            },

            {
                "question": "46 + 15 = ?",
                "punishments": [],
                "correct_answer": [61]
            },

            {
                "question": "45 + 17 = ?",
                "punishments": [],
                "correct_answer": [62]
            },

            {
                "question": "67 + 8 + 82 = ?",
                "punishments": [],
                "correct_answer": [157]
            },

            {
                "question": "27 + 7 + 23 = ?",
                "punishments": [],
                "correct_answer": [57]
            },

            {
                "question": "93 + 94 + 73 = ?",
                "punishments": [],
                "correct_answer": [260]
            }
        ]
    }
}`;
let quizzes = JSON.parse(jsonString);

/* HTML ELEMENTS USED IN SCRIPT */
let htmlTitle = document.getElementById("result_title");
let htmlResultTabBody = document.getElementById("result_tab_body");
let htmlFullResult = document.getElementById("full_result");
let htmlResultTabSum = document.getElementById("result_tab_sum");
let saveButton = document.getElementById("save_result");
let saveStatsButton = document.getElementById("save_stats");

let timers = JSON.parse(sessionStorage.getItem("timers"));
let userAnswers = JSON.parse(sessionStorage.getItem("answers"));

let questionsNumber = 0;
let quizTitle = "";
let questions = [];
let quizType = 0;
let quizPunishments = [];
let punishmentSum = 0;
let timeSpent = 0;
let quizId = sessionStorage.getItem("quiz");

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
    let elem = document.createElement("td");
    let x = setPunishment(type, i);
    punishmentSum += quizPunishments[x];

    if (type === 1) {
        if (x === 0) {
            elem.textContent = `Poprawna odpowiedź: +${quizPunishments[x]}s`;
            elem.setAttribute("style", "color: green;")
        }
        else if (x === 1) {
            elem.textContent = `Błędna odpowiedź: +${quizPunishments[x]}s`;
            elem.setAttribute("style", "color: #e2df06;")
        }
        else {
            elem.textContent = `Błędna odpowiedź: +${quizPunishments[x]}s`;
            elem.setAttribute("style", "color: red;")
        }
    }
    else if (type === 2) {
        if (x === 0) {
            elem.textContent = `Poprawna odpowiedź: +${quizPunishments[x]}s`;
            elem.setAttribute("style", "color: green;")
        }
        else {
            elem.textContent = `Błędna odpowiedź: +${quizPunishments[x]}s`;
            elem.setAttribute("style", "color: red;")
        }
    }

    node.appendChild(elem);
}

function setTable(type) {
    for (let i = 0; i < questionsNumber; i++) {
        let node = document.createElement("tr");
        let elem = document.createElement("td");
        elem.textContent = `Zadanie ${i + 1}`;
        node.appendChild(elem);

        elem = document.createElement("td");
        elem.textContent = userAnswers[i].toString();
        node.appendChild(elem);

        setPunishmentTabCell(node, type, i);
        htmlResultTabBody.appendChild(node);
    }
}

function sumTimers() {
    for (let timer of timers) {
        timeSpent += timer;
    }
}

function saveResult() {
    let x;
    if (quizId === "asd") {
        x = 0;
    }
    else if (quizId === "basic") {
        x = 1;
    }

    let results = JSON.parse(localStorage.getItem("results"));
    results[x].push(+(((timeSpent / 1000)+punishmentSum).toFixed(2)));
    localStorage.setItem("results", JSON.stringify(results));
    window.location.href = "main-page.html";
}

function saveResultWithStats() {
    let x;
    if (quizId === "asd") {
        x = 0;
    }
    else if (quizId === "basic") {
        x = 1;
    }

    let results = JSON.parse(localStorage.getItem("results"));
    results[x].push(+(((timeSpent / 1000)+punishmentSum).toFixed(2)));
    localStorage.setItem("results", JSON.stringify(results));

    let stats = JSON.parse(localStorage.getItem("stats"));
    stats[x].push(timers);
    localStorage.setItem("stats", JSON.stringify(stats));
    window.location.href = "main-page.html";
}

/* MAIN EXECUTION OF SCRIPT */
setQuizType()
htmlTitle.textContent = quizTitle;
setTable(quizType);
htmlResultTabSum.textContent = `+${punishmentSum}`;

if (punishmentSum > 0) {
    htmlResultTabSum.setAttribute("style", "color: red;");
}
else {
    htmlResultTabSum.setAttribute("style", "color: green;");
}

sumTimers();
htmlFullResult.textContent = `${((timeSpent / 1000)+punishmentSum).toFixed(2)}s`;

saveButton.setAttribute("onclick", "saveResult()");
saveStatsButton.setAttribute("onclick", "saveResultWithStats()");
