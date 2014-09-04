FIX-FAST-protocol
=================
Протокол подключения к Московской бирже на Erlang'е.
Адреса групп транслируемых данных для тестов ftp://ftp.moex.com/pub/FAST/Spectra/test_templates/internet_test/configuration.xml.
Подключение использует IGMP v.3 поэтому требется адрес источника 172.27.129.77.
Посмотреть результат подключения написанный на Erlang'е можно следующим образом:

1> A = ["172.27.129.77","239.192.7.27","24027"].

["172.27.129.77","239.192.7.27","24027"].

2> fast_sample:start(A).

