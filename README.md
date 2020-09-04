# Projekt_NPV-portfela-wierzytelnosci
Prognoza NPV portfela wierzytelności przy wykorzystaniu sieci neuronowych wykonany na zajęciach Uwr prowadzonych przez firmę Kruk sa.

Projekt:

Prognoza NPV portfela wierzytelności

Autorzy:

Krzysztof Bazner, 
Piotr Dzierza, 
Szymon Sowiński


Dane:

Dane dotyczące wierzytelności poszczególnych spraw.

    Składają się z dwóch tabel 
    
    cases - tabela spraw z charakterystykami z dnia przejęcia sprawy
		TOA - początkowe zadłużenie
		Principal - kapitał
		Interest - odsetki
		Other - inne
		TOA = Principal + Interest + Other 

    events - tabela zdarzeń opisująca zmienne behawioralne 
		Month - miesiąc
		NumberOfCalls - liczba wykonanych telefonów
		NumberOfCallsWithClient - liczba połączeń
		NumberOfLettersSent - liczba listów
		NumberOfVisits - liczba wizyt
		NumberOfLettersReceived - liczba listów otrzymanych
		NumberOfAgreementConcluded - liczba ugód wystawionych
		NumberOfAgreementSigned - liczba ugód podpisanych
		TransferToLegalProcess - przekazanie do sądu
		NumberOfPayment - liczba wpłat
		PaymentAmount - wartość wpłat



#Kolejność w jakiej zostały wykonywane pliki, ich charakter i output

- data_preprocessing.r 

	uzupełni i czyszczenie danych, sprawdzenie ich struktury i zgodności,
	uzupełnienie brakujących wartości
    
	OUTPUT: benchmark_pcase.rdata

- losowanie_portfeli.r

	podział każdej sprawy na portfele o różnych charakterystykach
	agregacja cech portfeli
    
	OUTPUT: final_benchmark.rdata
		events_agr.rdata

- model.r

	tworzenie modelu głównego (sieci neuronowej) oraz pomocniczego służącego do
	oceny jakości modelu,
	badanie własności uzyskanych wyników i parametrów uzyskanego modelu

- Projekt_prezentacja.pptx

	Zawiera wnioski z przeprowadzanych analiz, charakterystykę podejścia  
	płynące z nich konsekwencje, szansy i zagrożenia.
