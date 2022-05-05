-   2020-04-15 pojawiły się uwagi
-   2020-04-16 wyjaśnienie odnośnie bootstrap (uwaga 10)
-   2020-04-16 wyjaśnienie odnośnie statystyk i wyników (uwaga 11)
-   2020-04-27 zmiana deadline

Uwaga: drugie zadanie domowe będzie wymagało zrobienia pierwszego zadania

Do końca 4 maja 2020 naszego czasu.

Należy przygotować aplikację webową, który pozwala na rozwiązywanie quizów algebraicznych. Quiz składa się z wprowadzenia, listy pytań i odpowiedzi oraz informacji o punktacji (ile sekund kosztuje podanie błędnej odpowiedzi na każde z pytań).

Przykład quizu:

_Wstęp:_

Liczyć każdy może

_Pytania i odpowiedzi:_

-   _2 + 3 = 5_
-   _2 - (-24 : 4) = 8_

_Punktacja:_

-   błędna odpowiedź na pytanie pierwsze to 4 sekundy, a na pytanie drugie to 10 sekund

1.  Zaproponuj format zapisu quizu w postaci pliku JSON
2.  Przygotuj stronę, która będzie zawierała wstęp oraz będzie pozwalała użytkownikowi rozwiązać cały quiz. Rozwiązywanie quizu powinno się uruchamiać przyciskiem "Start". W danym momencie powinno wyświetlać się tylko jedno pytanie. Powinno się dać przejść do kolejnego pytania, nie udzielając odpowiedzi na pytanie poprzednie. Powinno się dać cofnąć do poprzedniego pytania. Nie powinno się dać zakończyć quizu nie udzielając odpowiedzi na wszystkie pytania. Quiz powinno się kończyć przyciskiem "Stop", który jest aktywny tylko gdy są udzielone odpowiedzi na wszystkie pytania.
3.  W trakcie rozwiązywania quizu powinno się dać czytać wstęp
4.  Strona powinna zapisywać statystyki dla pojedynczego rozwiązania quizu: czas spędzony nad każdym pytaniem.
5.  Powinno się móc dać zapisać same wyniki, albo też wyniki ze statystykami w localStorage
6.  Całkowita punktacja za quiz to całkowity czas spędzony nad quizem plus kary za błędne odpowiedzi.

1.  Program należy napisać w TypeScripcie, nie korzystając z dodatkowych bibliotek (jeśli ktoś sam napisał swoją bibliotekę i ona jest w TypeScripcie i chce tłumaczyć prowadzącemu dlaczego ona jest dobra i nie zawiera zbędnych elementów i nie łamie innych zasad to może z niej korzystać)
2.  Na razie nie używamy serwerów, czyli strona ma być zestawem plików.
3.  Trzeba zrobić jeden quiz w swoim formacie JSONowym. Quiz powinien zawierać przynajmniej 4 pytania. Można zrobić więcej quizów i pozwolić użytkownikowi wybierać quiz na początku. Quizy mają być przechowywane w kodzie programu.
4.  Jak sobie wyobrażam działanie tego programu:
    -   Zaczyna się na jakiejś stronie startowej, na której można wybrać "Zacznij nowy quiz" i ewentualnie listę najlepszych wyników zapisanych w localstorage,
    -   Po wybraniu "Zacznij" przechodzi się do innego widoku (może innej strony, ale niekoniecznie), koniecznie wyświetla się wstęp i pierwsze pytanie, miejsce na odpowiedź, przyciski do przejścia do poprzedniego i następnego pytania oraz przycisk zakończenia; jeśli jest wolne miejsce, to wyświetla się kara za złą odpowiedź na wyświetlone pytanie, stoper odliczający czas i numer pytania oraz liczba pytań w quizie, a także przycisk anulowania quizu, który pozwala go zakończyć bez zapisywania wyniku,
    -   zakończenie quizu powoduje przejście do widoku z wynikiem quizu, z zaznaczeniem które odpowiedzi były poprawne, informacją o karach i wyborem czy zapisać tylko wynik, czy wynik ze statystykami,
    -   po zapisaniu wyniku wraca się do widoku startowego.
5.  Odpowiedzi na pytania są wyłącznie liczbami całkowitymi.
6.  Oceniane będzie zarówno działanie aplikacji, w tym jej użyteczność, jak i jakość kodu.
7.  Quizy powinno się dać rozwiązywać zarówno na komputerze, jak i telefonie, czyli należy obsługiwać różne wielkości ekranów.
8.  To jest zadanie domowe, czyli coś poważniejszego niż zadanie labowe. Pewnie tak, jak do zadań labowych, do niektórych rozwiązań będą uwagi. Część z tych uwag będziemy uznawać za "**istotne**", i jeśli ktoś będzie miał więcej niż 3 istotne uwagi, to nie zaliczy zadania - czyli i tak będzie je musiał poprawić później, ale nie będzie miał szans na egzamin w pierwszym terminie.
9.  Z drugiej strony, jeśli ktoś przedstawi jakieś zachwycające - co specjalnie nie jest szczegółowo określone - rozwiązanie, to nagrodą jest pół oceny wyżej z egzaminu. Zachwycić może elegancja kodu, wygląd, wymyślone rozszerzenia, ale ta lista nie jest zamknięta.
10.  Bootstrap ma obszerne fragmenty w JavaScripcie. Wolno go używać do CSS jeśli się potrafi dokładnie wyjaśnić jak działają wybrane przez Was komponenty.
11.  Statystki wyłącznie zapisujemy w localstorage, a (na ten moment) prezentujemy tylko wyniki