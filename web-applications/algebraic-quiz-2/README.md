15 czerwca - wyjaśnienie: powinna być jedna aplikacja (serwer) i ona serwuje .js i .html i .css dla użytkownika, czyli cała komunikacja "_Aplikacja powinna pobierać listę quizów a następnie pojedyncze quizy z serwera WWW w postaci JSONów a następnie powinna odsyłać na ten sam serwer odpowiedzi..._" ma działać podobnie do tej komunikacji np w extodo

9 czerwca - \`npm run createdb\` (zadanie 9)

9 czerwca - wyjaśnienie dotyczące "integracji" z zadaniem 1: zakończenie quizu powoduje przejście do widoku z wynikiem quizu, z zaznaczeniem które odpowiedzi były poprawne, informacją o karach ~i wyborem czy zapisać tylko wynik, czy wynik ze statystykami,~ **użytkownik nie powinien mieć wyboru**

5 czerwca - dopisek o skryptach (pkt. 8 zadania)

Do końca 21 czerwca 2020 naszego czasu.

Trzeba napisać aplikację webową, czyli serwer HTTP.

Należy przygotować mechanizm pozwalający użytkownikom logować się (login i hasło) do aplikacji z Zadania domowego 1, rozwiązywać quizy, zapisywać wyniki, przeglądać rozwiązane quizy.

Dodatkowo należy przygotować testy w Selenium.

I zmodyfikować package.json

1.  Przygotuj mechanizm logowania, zmiany własnego hasła i wylogowywania ze strony. Dane użytkowników powinny być przechowywane w bazie danych Nie musisz przygotowywać mechanizmu zakładania kont.
2.  Po zmianie hasła sesje użytkownika, który zmienił hasło powinny być wylogowywane. Napisz test tego zachowania w Selenium. W tym celu możesz na przykład:
    1.  zapisać ciasteczka jednej sesji,
    2.  usunąć je z przeglądarki,
    3.  stworzyć drugą sesję,
    4.  zmienić hasło,
    5.  wczytać ciasteczka z pierwszej sesji,
    6.  sprawdzić, że jesteś wylogowany.
3.  Aplikacja powinna pobierać listę quizów a następnie pojedyncze quizy z serwera WWW w postaci JSONów a następnie powinna odsyłać na ten sam serwer odpowiedzi (wszystkie odpowiedzi do quizu na raz a nie pojedynczo) oraz statystyki w postaci procentowego czasu spędzonego nad konkretnym pytaniem (np. pyt1: 10%, pyt2: 30%, pyt3: 60%). Przygotuj test tego zachowania w Selenium.
4.  Quizy powinny być zapisane w bazie danych, nie trzeba do ich edycji przygotowywać interfejsu.
5.  Rozwiązania quizów powinny być oceniane po stronie serwera. Serwer powinien, w oparciu o to kiedy przesłał do przeglądarki JSONa z pytaniami oraz kiedy odebrał JSONa z odpowiedziami policzyć ile czasu zajęła użytkownikowi odpowiedź na każde z pytań.
6.  Nie powinno się dać dwukrotnie rozwiązać tego samego quizu, zadbaj o komunikat o błędzie. Przygotuj test tego zachowania w Selenium.
7.  Użytkownik powinien móc zobaczyć wyniki rozwiązanych przez siebie quizów:
    1.  które odpowiedzi były poprawne a które nie
    2.  jakie były poprawne odpowiedzi
    3.  jakie są statystyki kilku najlepszych użytkowników (np. top 5), którzy już rozwiązali quiz
    4.  ile średnio zajmuje czasu poprawna odpowiedź na każde z pytań
8.  Technicznie: do package.json należy dopisać skrypt, który po npm run test uruchomi testy selenium, a następnie po sobie posprząta. Należy też dodać skrypt uruchamiający. Jeśli jest potrzebny, to należy też zrobić skrypt budujący.
9.  Polecenie \`npm run createdb\` powinno utworzyć potrzebną bazę i założyć w niej dwa konta użytkowników (żeby było łatwo testować) user1/user1 i user2/user2.

1.  Program należy napisać w TypeScripcie, korzystając z expressa.