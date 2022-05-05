/**
 * Script handling 'main-page.html'.
 */

const jsonString: string = `{
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
                "question": "Każdy ze 100 studentów wybiera losowo i niezależnie od pozostałych jedną stronę z polskiego wydania książki Wprowadzenie do algorytmów Cormena, Leisersona, Rivesta i Steina. Jakie jest prawdopodobieństwo, że pewnych dwóch studentów wybrało tę samą stronę. Odpowiedź wyraź w procentach zaokrąglając do cyfr jedności, pomijając znak '%' na końcu.",
                "punishments": [[75, 89], [25, 75]],
                "correct_answer": [89, 100]
            },

            {
                "question": "Jaka wartość pieniężna w monetach jednozłotowych zmieści się w standardowej wielkości wiadrze?",
                "punishments": [[1000, 5000], [10000, 50000]],
                "correct_answer": [5000, 10000]
            },

            {
                "question": "Ile chococino z automatu Cafe+co na wydziale MIMUW trzeba wypić żeby spełnić dzienne zapotrzebowanie na cukier wskazywane przez GDA?",
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
const quizzes = JSON.parse(jsonString);

/* HTML ELEMENTS USED IN SCRIPT */
const quizTitle = document.getElementById("title");
const quizDescription = document.getElementById("description");
const quizExampleQuestion = document.getElementById("example_question");
const quizExampleAnswer = document.getElementById("example_answer");
const leaderboardBody = document.getElementById("leaderboard_body");
const selectQuiz = document.getElementById("select_quiz") as HTMLSelectElement;
const beginButton = document.getElementById("begin") as HTMLButtonElement;

/* 'GENERIC' FUNCTIONS */
function setExampleQuestion(question) {
    quizExampleQuestion.textContent = "";
    let node = document.createElement("strong");

    node.textContent = "Przykład:";
    quizExampleQuestion.appendChild(node);

    node = document.createElement("p");
    node.textContent = question;

    quizExampleQuestion.appendChild(node);
}

function setExampleAnswer(answer, punishments) {
    quizExampleAnswer.textContent = "";
    let node = document.createElement("strong");

    node.textContent = `Odpowiedź użytkownika: ${answer}`;
    quizExampleAnswer.appendChild(node);

    node = document.createElement("hr");
    quizExampleAnswer.appendChild(node);

    node = document.createElement("p");
    node.textContent = punishments;

    quizExampleAnswer.appendChild(node);
}

function setLeaderboard(results) {
    leaderboardBody.textContent = "";
    const x = Math.min(5, results.length);
    results = results.sort((n1,n2)=> n1 > n2);
    for (let i = 0; i < x; i++) {
        const node = createLeaderboardTableRow(i + 1, results[i]);
        leaderboardBody.appendChild(node);
    }
}

function createLeaderboardTableRow(position, result) {
    const node = document.createElement("tr");
    let elem = document.createElement("td");
    elem.textContent = position.toString();
    node.appendChild(elem);

    elem = document.createElement("td");
    elem.textContent = `${result}s`;
    node.appendChild(elem);

    return node;
}

/**
 * Function for '<select>' element determining which quiz should be displayed.
 */
function displayQuiz() {
    if (selectQuiz.value === "asd") {
        setAsd();
    }
    else if (selectQuiz.value === "basic") {
        setBasic();
    }
    else {
        setBlank();
    }
}

/* 'QUIZ SPECIFIC' FUNCTIONS */
function setAsd() {
    quizTitle.textContent = quizzes.asd.title;
    quizDescription.textContent = quizzes.asd.description;
    setExampleQuestion(quizzes.asd.example);
    setExampleAnswer(quizzes.asd.example_answer, quizzes.asd.example_punishments);
    setLeaderboard(JSON.parse(localStorage.getItem('results'))[0]);

    sessionStorage.setItem('quiz', 'asd');
    beginButton.disabled = false;
}

function setBasic() {
    quizTitle.textContent = quizzes.basic.title;
    quizDescription.textContent = quizzes.basic.description;
    setExampleQuestion(quizzes.basic.example);
    setExampleAnswer(quizzes.basic.example_answer, quizzes.basic.example_punishments);
    setLeaderboard(JSON.parse(localStorage.getItem('results'))[1]);

    sessionStorage.setItem('quiz', 'basic');
    beginButton.disabled = false;
}

function setBlank() {
    quizTitle.textContent = "Tytuł";
    quizDescription.textContent = "Opis quizu.";
    setExampleQuestion("Przykładowe pytanie.");
    setExampleAnswer("", "Kary za błędną odpowiedź.");
    setLeaderboard([]);

    sessionStorage.setItem("quiz", "blank");
    beginButton.disabled = true;
}

/* MAIN EXECUTION OF SCRIPT */
if (localStorage.getItem('results') === null) {
    localStorage.setItem('results', JSON.stringify([[],[]]));
}

if (localStorage.getItem('stats') === null) {
    localStorage.setItem('stats', JSON.stringify([[],[]]));
}

setBlank(); // On page load 'blank' quiz is displayed.
beginButton.setAttribute('onclick', "window.location.href = 'quiz.html';");
selectQuiz.setAttribute('onchange', "displayQuiz()");
