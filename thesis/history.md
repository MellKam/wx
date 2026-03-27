Dlaczego JavaScript jest problematycznym standardem wykonywania kodu w środowisku WWW?

🔗 The Weird History of JavaScript
https://www.youtube.com/watch?v=Sh6lK57Cuk4

Twórcy technologii internetowych w latach 90. doskonale zdawali sobie sprawę z ogromnego potencjału sieci WWW jako platformy aplikacyjnej. Trudno jednak było wówczas przewidzieć, że po trzech dekadach JavaScript stanie się najpopularniejszym językiem programowania na świecie, wykorzystywanym do tworzenia rozbudowanych aplikacji biznesowych liczących miliony linii kodu. Niewielu mogło przypuszczać, że JavaScript będzie wykonywany po stronie serwera lub że wokół niego powstanie ekosystem obejmujący miliony bibliotek i frameworków.

Z biegiem czasu JavaScript znacząco ewoluował jako narzędzie programistyczne. Wprowadzenie kompilacji Just-In-Time (JIT) umożliwiło istotne przyspieszenie wykonywania kodu, natomiast nowe standardy języka oraz przeglądarkowe interfejsy API znacznie rozszerzyły jego możliwości. Pomimo tego w języku nadal istnieją fundamentalne ograniczenia wynikające z pierwotnych założeń projektowych. Należą do nich m.in. dynamiczne typowanie, automatyczne zarządzanie pamięcią (garbage collection) oraz kosztowne abstrakcje runtime’owe. Ograniczenia te można zaakceptować lub próbować je częściowo kompensować poprzez narzędzia nadbudowane nad językiem, takie jak TypeScript.

Nieprzewidywalność wykonania

🔗 Lin Clark: A Cartoon Intro to WebAssembly | JSConf EU
https://www.youtube.com/watch?v=HktWin_LPf4

JavaScript został zaprojektowany jako język wysokiego poziomu o dynamicznym systemie typów. W konsekwencji:

Każda operacja wymaga weryfikacji typów w czasie wykonania. Kompilatory JIT mogą optymalizować fragmenty kodu, zakładając określone typy danych, jednak w przypadku niespełnienia tych założeń konieczny jest powrót do wolniejszego trybu interpretacji.

Mechanizm garbage collection może zostać uruchomiony w dowolnym momencie, powodując nieprzewidywalne przerwy w wykonaniu programu. Jest to szczególnie problematyczne w aplikacjach wymagających niskich opóźnień, takich jak gry komputerowe czy systemy przetwarzania audio i wideo.

Dynamiczne wiązanie metod (dynamic dispatch) wprowadza dodatkowy narzut czasowy, ponieważ wybór konkretnej implementacji metody następuje dopiero w czasie wykonania i wymaga analizy łańcucha prototypów.

W rezultacie dwa uruchomienia tego samego skryptu mogą charakteryzować się odmienną wydajnością. Problem ten jest dodatkowo potęgowany przez fakt istnienia wielu przeglądarek, wykorzystujących różne silniki JavaScript oraz odmienne strategie optymalizacji. Kod, który działa poprawnie w jednej przeglądarce, może wykazywać znaczne spadki wydajności w innej.

Google Native Client

Naturalnym dążeniem programistów było wykorzystanie znanych i sprawdzonych języków programowania do tworzenia aplikacji internetowych, zamiast polegania na dynamicznym języku skryptowym, jakim jest JavaScript.

🔗 Google I/O 2012 – Life of a Native Client Instruction
https://www.youtube.com/watch?v=KOsJIhmeXoc

Z tego punktu widzenia uruchamianie w przeglądarce kodu natywnego skompilowanego z języków takich jak C lub C++ wydawało się atrakcyjnym rozwiązaniem. W 2011 roku Google zaproponowało w tym celu technologię Native Client (NaCl).

Bezpieczeństwo systemu operacyjnego a bezpieczeństwo WWW

W systemach operacyjnych użytkownik jest ostrzegany przed instalacją aplikacji pochodzących z nieznanych źródeł, ponieważ system nie jest w stanie odróżnić kodu bezpiecznego od złośliwego. Uruchomienie aplikacji wiąże się z pełnym dostępem do zasobów systemowych i przenosi odpowiedzialność za bezpieczeństwo na użytkownika końcowego.

Model bezpieczeństwa przeglądarek internetowych jest zasadniczo odmienny. Kod stron internetowych uruchamiany jest w izolowanym środowisku (tzw. sandboxie), które ogranicza dostęp do zasobów systemowych i zapewnia separację pomiędzy poszczególnymi stronami. Dzięki temu możliwe jest bezpieczne wykonywanie kodu pochodzącego od stron trzecich.

Kod natywny domyślnie posiada szeroki dostęp do sprzętu i systemu operacyjnego. Rozwiązanie zaproponowane w Native Client polegało na uruchamianiu dodatkowych procesów oraz statycznej analizie kodu w celu eliminacji potencjalnie niebezpiecznych instrukcji. Podejście to było jednak bardzo złożone, trudne w utrzymaniu oraz nie zostało zaakceptowane przez innych producentów przeglądarek.

Początkowo NaCl wykorzystywał bezpośrednio kod natywny, co ograniczało go do jednej architektury sprzętowej. W późniejszym etapie wprowadzono język pośredni, kompilowany do kodu natywnego po stronie klienta, co zwiększało narzut wykonania i nadal nie rozwiązywało kluczowych problemów bezpieczeństwa.

Technologia Native Client spotkała się z szeroką krytyką i ostatecznie została wycofana w 2020 roku, pozostając rozwiązaniem specyficznym dla przeglądarki Google Chrome.

asm.js

🔗 From asm.js to Wasm with Emscripten creator Alon Zakai
https://www.youtube.com/watch?v=cv5uQ_hQVE0

Alternatywne podejście zaproponował zespół Mozilla. Zamiast uruchamiać kod natywny bezpośrednio w przeglądarce, zaproponowano wykorzystanie JavaScriptu jako celu kompilacji, przy jednoczesnym ograniczeniu generowanego kodu do ściśle zdefiniowanego podzbioru języka. Podzbiór ten miał być łatwy do analizy i optymalizacji przez silniki JavaScript.

asm.js wprowadzał jawne oznaczanie typów zmiennych oraz eliminował dynamiczne alokacje pamięci, redukując tym samym wpływ garbage collectora. Przykładowa funkcja w asm.js:

function add(a, b) {
  "use asm";
  a = a | 0;
  b = b | 0;
  return (a + b) | 0;
}


Dyrektywa "use asm" informowała silnik JavaScript o możliwości zastosowania agresywniejszych optymalizacji. Operator bitowy OR (|) był wykorzystywany do wymuszenia konwersji typu na 32-bitową liczbę całkowitą, co było istotne, ponieważ JavaScript domyślnie przechowuje wszystkie liczby jako wartości typu float64.

asm.js intensywnie wykorzystywał również tablice typowane (typed arrays), które umożliwiały częściową kontrolę nad pamięcią i symulowały niskopoziomowe operacje znane z języków systemowych.

Pomimo tego, że asm.js stanowił istotny dowód koncepcji i wykazał możliwość osiągnięcia wysokiej wydajności w przeglądarce, rozwiązanie to miało poważne wady. Kod skompilowany do asm.js był znacznie większy objętościowo niż kod natywny, co prowadziło do dłuższego czasu pobierania i parsowania. Dodatkowo brak natywnych API wymuszał emulację funkcjonalności takich jak grafika czy system plików za pomocą technologii webowych (WebGL, IndexedDB, WebAudio).

„And it ran, but obviously it was still kind of a hack”
— Alon Zakai