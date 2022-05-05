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
const quizzes = JSON.parse(jsonString);

/* HTML ELEMENTS USED IN SCRIPT */
const htmlTitle = document.getElementById("title");
const htmlDescription = document.getElementById("description");
const quizProgress = document.getElementById("quiz_progress");
const htmlQuestionNumber = document.getElementById("question_number");
const htmlQuestion = document.getElementById("question");
const answer = document.getElementById("ans") as HTMLInputElement;
const nextButton = document.getElementById("next") as HTMLButtonElement;
const prevButton = document.getElementById("previous") as HTMLButtonElement;

/* VARIABLES WHICH WILL BE SET ACCORDING TO QUIZ TYPE */
let questions = [];
let questionsNumber = 0;
let quizTitle = "";
let quizDescription = "";

/**
 * Function setting 'quiz-describing' variables according to value set in 'sessionStorage'.
 */
function setQuiz() {
    if (sessionStorage.getItem("quiz") === "asd") {
        quizTitle = quizzes.asd.title;
        quizDescription = quizzes.asd.description;
        questionsNumber = quizzes.asd.questions_no;
        questions = quizzes.asd.questions;
    }
    else if (sessionStorage.getItem("quiz") === "basic") {
        quizTitle = quizzes.basic.title;
        quizDescription = quizzes.basic.description;
        questionsNumber = quizzes.basic.questions_no;
        questions = quizzes.basic.questions;
    }
}

/**
 * Function checking if answers for all questions except last have been provided.
 */
function checkAnswersExceptLast() {
    for (let i = 0; i < questionsNumber - 1; i++) {
        if (answers[i] === "") {
            return false;
        }
    }

    return true;
}

/**
 * Function setting progress bar according to which questions have been answered.
 */
function setProgress() {
    quizProgress.innerHTML = "";
    const width = `width:${100 / questionsNumber}%`

    for (let i = 1; i <= questionsNumber; i++) {
        const node = document.createElement("div");
        const text = document.createTextNode(i.toString());
        node.appendChild(text);

        if (answers[i-1] === "") {
            node.setAttribute("class", "progress-bar");
        }
        else {
            node.setAttribute("class", "progress-bar bg-success");
        }

        node.setAttribute("role", "progressbar");
        node.style.width = `${100 / questionsNumber}%`;
        quizProgress.appendChild(node);
    }
}

/**
 * Function sets page view based on question number (@param k).
 */
function setView(k) {
    htmlQuestionNumber.textContent = "Zadanie " + k.toString();
    htmlQuestion.textContent = (questions[k-1]).question;
    answer.value = answers[k-1];
    setProgress();

    if (k === 1) {
        prevButton.disabled = true;
        nextButton.textContent = "Następne pytanie";
        nextButton.setAttribute("onclick", "nextClick()");
        answer.setAttribute("onkeyup", "");
        nextButton.disabled = false;
    }
    else if (k === questionsNumber) {
        prevButton.disabled = false;
        nextButton.textContent = "Zakończ";
        nextButton.setAttribute("onclick", "endQuiz()");

        if (checkAnswersExceptLast() === true) {
            if (answers[questionsNumber-1] === "") {
                nextButton.disabled = true;
            }
            else {
                nextButton.disabled = false;
            }

            answer.setAttribute("onkeyup", "lastAnswer()")
        }
        else {
            answer.setAttribute("onkeyup", "")
            nextButton.disabled = true;
        }
    }
    else {
        prevButton.disabled = false;
        nextButton.textContent = "Następne pytanie";
        nextButton.setAttribute("onclick", "nextClick()");
        answer.setAttribute("onkeyup", "");
        nextButton.disabled = false;
    }
}

/* FUNCTIONS FOR BUTTONS AND INPUT */
function lastAnswer() {
    if ((/^(0|[1-9][0-9]*)$/).test(answer.value) === true) {
        nextButton.disabled = false;
    }
    else {
        nextButton.disabled = true;
    }
}

function nextClick() {
    const timer = new Date().getTime();
    timers[currentQuestion - 1] += timer - start;
    start = timer;

    if ((/^(0|[1-9][0-9]*)$/).test(answer.value) === true) {
        answers[currentQuestion-1] = answer.value;
    }
    else {
        answers[currentQuestion-1] = "";
    }

    currentQuestion += 1;
    setView(currentQuestion);
}

function prevClick() {
    const timer = new Date().getTime();
    timers[currentQuestion - 1] += timer - start;
    start = timer;

    if ((/^(0|[1-9][0-9]*)$/).test(answer.value) === true) {
        answers[currentQuestion-1] = answer.value;
    }
    else {
        answers[currentQuestion-1] = "";
    }

    currentQuestion -= 1;
    setView(currentQuestion);
}

function endQuiz() {
    const timer = new Date().getTime();
    timers[currentQuestion - 1] += timer - start;

    answers[currentQuestion-1] = answer.value;

    sessionStorage.setItem('answers', JSON.stringify(answers));
    sessionStorage.setItem('timers', JSON.stringify(timers));

    window.location.href = 'end-quiz.html';
}

/* MAIN EXECUTION OF SCRIPT */
setQuiz();

const answers: string[] = [];
const timers = [];
for (let i = 1; i <= questionsNumber; i++) {
    answers.push("");
    timers.push(0);
}

let currentQuestion = 1;

htmlTitle.textContent = quizTitle;
htmlDescription.textContent = quizDescription;
setView(currentQuestion);

prevButton.setAttribute("onclick", "prevClick()");

let start = new Date().getTime();
