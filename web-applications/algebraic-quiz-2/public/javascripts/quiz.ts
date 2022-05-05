var quiz;

/* HTML ELEMENTS USED IN SCRIPT */
const htmlTitle = document.getElementById("title");
const htmlDescription = document.getElementById("description");
const quizProgress = document.getElementById("quiz_progress");
const htmlQuestionNumber = document.getElementById("question_number");
const htmlQuestion = document.getElementById("question");
const answer = document.getElementById("ans") as HTMLInputElement;
const nextButton = document.getElementById("next") as HTMLButtonElement;
const prevButton = document.getElementById("previous") as HTMLButtonElement;


/**
 * Function checking if answers for all questions except last have been provided.
 */
function checkAnswersExceptLast() {
    for (let i = 0; i < quiz.questions_no - 1; i++) {
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
    const width = `width:${100 / quiz.questions_no}%`

    for (let i = 1; i <= quiz.questions_no; i++) {
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
        node.style.width = `${100 / quiz.questions_no}%`;
        quizProgress.appendChild(node);
    }
}

/**
 * Function sets page view based on question number (@param k).
 */
function setView(k) {
    htmlQuestionNumber.textContent = "Zadanie " + k.toString();
    htmlQuestion.textContent = (quiz.questions[k-1]).question;
    answer.value = answers[k-1];
    setProgress();

    if (k === 1) {
        prevButton.disabled = true;
        nextButton.textContent = "Następne pytanie";
        nextButton.setAttribute("onclick", "nextClick()");
        answer.setAttribute("onkeyup", "");
        nextButton.disabled = false;
    }
    else if (k === quiz.questions_no) {
        prevButton.disabled = false;
        nextButton.textContent = "Zakończ";
        nextButton.setAttribute("onclick", "endQuiz()");

        if (checkAnswersExceptLast() === true) {
            if (answers[quiz.questions_no-1] === "") {
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

function calculatePercents(values) {
    let sum = 0;

    for (let value of values) {
        sum += value;
    }

    let percent;
    let percents = [];
    let currentPercentsSum = 0;

    console.log(values);
    console.log(sum);

    for (let i = 0; i < values.length - 1; i++) {
        percent = Math.floor((values[i] / sum) * 100);
        currentPercentsSum += percent;
        percents.push(percent);
    }

    percents.push(100 - currentPercentsSum); // calculate last percent this way to ensure that all of them sum to 100

    return percents;
}

/* Function called when user ends quiz */
function endQuiz() {
    const timer = new Date().getTime();
    timers[currentQuestion - 1] += timer - start;

    answers[currentQuestion-1] = answer.value;
    
    const stats = calculatePercents(timers);

    const answersAndStats = JSON.stringify({answers: answers, stats: stats});

    const resultsRequest = new XMLHttpRequest();

    resultsRequest.open("POST", "/json/result");
    resultsRequest.setRequestHeader("Content-Type", "application/json");
    resultsRequest.send(answersAndStats);

    resultsRequest.onload = function () {
        console.log(resultsRequest.response);
        console.log(answersAndStats);
        window.location.href = '/';
    }
}

/* MAIN EXECUTION OF SCRIPT */
var currentQuestion = 1;
const answers: string[] = [];
const timers = [];
let quizRequest = new XMLHttpRequest();

quizRequest.open('GET', '/json/quiz');
quizRequest.responseType = 'json';
quizRequest.send();

quizRequest.onload = function () {
    quiz = quizRequest.response;
    console.log(quiz);
    htmlTitle.textContent = quiz.title;
    htmlDescription.textContent = quiz.description;

    for (let i = 1; i <= quiz.questions_no; i++) {
        answers.push("");
        timers.push(0);
    }

    setView(currentQuestion);
}



prevButton.setAttribute("onclick", "prevClick()");

let start = new Date().getTime();
