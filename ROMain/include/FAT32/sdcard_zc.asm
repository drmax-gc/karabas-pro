	module sdcard_zc
	; by koshir

CONF    EQU #77;порт конфигурации
DATA    EQU #57;порт данных

CMD_1   EQU %01000000+1; инициализация
CMD_12  EQU %01000000+12;остановка передачи
CMD_16  EQU %01000000+16;задание размера блока
CMD_18  EQU %01000000+18;много-секторное чтение
CMD_25  EQU %01000000+25;много-секторная запись
ACMD_41 EQU %01000000+41;инициализация (только SDC)
CMD_55  EQU %01000000+55;команда префикс к ACMD
CMD_58  EQU %01000000+58;чтение OCR

CMD00   DB %01000000+0:DS 4:DB #95;программный сброс
CMD08   DB %01000000+8,0,0,1,#AA,#87;напряжение карты (только для SDCv2)
CMD16   DB %01000000+16,0,0,2,0,#FF;задание размера блока (512б)

BLKT    db 0; тип адресации (0 — байтовая, 1 — блоковая)

;выставление CS в 1
CSH	PUSH BC,AF

	LD BC,CONF
	LD A,%00000011
	OUT (C),A

	LD BC,DATA
	LD A,#FF
	OUT (C),A

	POP AF,BC
	RET

CSL     ;выставление CS в 0
        PUSH BC,AF

        LD BC,CONF
        LD A,%00000001
        OUT (C),A

        LD BC,DATA
        LD A,#FF
        OUT (C),A

        POP AF,BC
        JP WAIT


SNB     ;пауза в 128 тактирований
        PUSH BC,AF
        LD B,16
SnB     XOR A
        IN A,(DATA)
        DJNZ SnB
        POP AF,BC
        RET


;-------
WTBY    ;ожидание снятия BUSY
        PUSH BC,AF
        LD BC,DATA
        IN A,(C)
        OR A
        JR Z,$-3
        POP AF,BC
        RET

WTDO    ;ожидание DO
        PUSH BC
        LD BC,DATA
        IN A,(C)
        CP #FF
        JR Z,$-4
        POP BC
        RET

WAIT    ;ожидаем пустой шины
        PUSH BC,AF
        LD BC,DATA
        IN A,(C)
        INC A
        JR NZ,$-3
        POP AF,BC
        RET

;-------
SDOFF   ;выключаем питание карты
        XOR A
        OUT (CONF),A
        OUT (DATA),A
        RET

;-------
CMDo    CALL CSH
        CALL CSL

CMDx    PUSH BC
        LD BC,DATA
        OUT (C),A
        XOR A
        OUT (C),A
        OUT (C),A
        OUT (C),A
        OUT (C),A
        DEC A
        OUT (C),A
        POP BC
        RET

CMD1    ;запуск инициализации (только для MMC)
        LD A,CMD_1
        CALL CMDo
        JP RESP

CMD12   ;завершение передачи данных (вызывается при завершении много-секторного чтения)
        LD A,CMD_12
        CALL CMDx
        XOR A
        IN A,(DATA)
        JP RESP

CMD55   ;вызывается перед ACMD<n>
        LD A,CMD_55
        CALL CMDo
        JP RESP

CMD58   ;чтение OCR
        LD A,CMD_58
        CALL CMDo
        JP RESP


;-------
ACMD41  ;запуск инициализации (только для SDC)
        CALL CMD55
        CALL CSH
        CALL CSL

        LD BC,DATA
        LD A,ACMD_41
        OUT (C),A
        LD L,0
        OUT (C),H
        OUT (C),L
        OUT (C),L
        OUT (C),L
        DEC L
        OUT (C),L
        JP RESP


;-------
CMD18   ;запуск много-секторного чтения (завершение производится по cmd12)
        LD A,CMD_18
CMDz    CALL CSH
        CALL CSL

        PUSH HL,DE,BC

        LD L,C
        LD H,B

        LD C,A
        LD A, (BLKT)
        OR A
        JR NZ,CMzz

        EX DE,HL
        ADD HL,HL
        EX DE,HL
        ADC HL,HL
        LD H,L
        LD L,D
        LD D,E
        LD E,A

CMzz    LD A,C
        LD BC,DATA
        OUT (C),A
        OUT (C),H
        OUT (C),L
        OUT (C),D
        OUT (C),E
        LD A, #FF
        OUT (C),A
        POP BC,DE,HL
        JP RESP



CMD25   ;запуск много-секторной записи (завершение производится по стоп токену!)
        LD A,CMD_25
        JR CMDz

;-------
CMDi6   ;задание размера блока (должно быть всегда 512б)
        LD HL,CMD16
        JR CMD

CMD8    ;узнать какое напряжение нужно карте (только для SDCv2)
        LD HL,CMD08
        JR CMD

CMD0    ;программный сброс карты
        LD HL,CMD00
CMD     CALL CSH
        CALL CSL
        LD BC,DATA
        .6 OUTI

RESP    ;ожидание cmd. Resp. (в течении 80 тактирований)
        PUSH DE,BC
        LD BC,DATA
        LD D,10
RESp    IN A,(C)
        BIT 7,A
        JR Z,REZ
        DEC D
        JR NZ,RESp
        INC D
REZ     POP BC,DE
        RET


;-------
DRESP   ;ожидание Data Response
        PUSH BC,AF

        LD BC,DATA

	IN A,(C)
        CP #FF
        JR Z,$-4
        AND #1F

        CP 5
        JR NZ,ER;если Data Resp. принят без ошибок, то идем дальше (%0XXX1, xxx – биты статуса [%010 – данные приняты])

	IN A,(C)
        OR A
        JR Z,$-3;ждем пока пройдет busy

	POP AF,BC
        RET


ER      ;подсвечиваем бордер кодом ошибки и вешаемся (предварительно дав токен конца данных, дабы не гробилась пага целиком)
        AND 7
        OUT (254),A
        CALL SDND;даем стоп токен
        CALL SDOFF;вырубаем питание карты
        JR $;здесь можно сделать вылет по ошибке, с сообщением, что носитель требует замены


;на выходе: A = 0 – инициализация прошла успешно, A = 1 — карта не поддерживается/не обнаружена
init

SD_INIT CALL CSH

        LD DE,512+10
        CALL CYCL;выгребаем 512+10 байт, на всякий случай, мало ли карта что-то не отдала/приняла

        LD DE,8000;8к циклов ожидания
SDWT    DEC DE
        LD A,D
        OR E
        JP Z,NOSD

        CALL CMD0;команда на инициализацию карты
        JR NZ,SDWT;висим в цикле, пока не будет получен cmd. Resp. (в отведенные 80 тактирований)
        DEC A
        JP NZ,SDWT;висим в цикле, пока не получим ответ без ошибок и с выставленным Idle State флагом (bit 0)

;если все прошло успешно, то карта переключается в SPI режим

        CALL CMD8;запрос на поддерживаемое напряжение питания (только для SDCv2)
        PUSH AF
        IN E,(C)
        IN E,(C)
        IN H,(C)
        IN L,(C)
        POP AF
        JR NZ,SDWT;cmd. Resp. получен?
        BIT 2,A
        JR Z,SDNEW;если нет ошибки Illegal command (бит 2), то перед нами SDCv2

;-------
;cmd08 не поддерживается, соотв. перед нами либо SDCv1, либо MMC
SDOLD   LD DE,8000;8к циклов ожидания
AA      DEC DE
        LD A,D
        OR E
        JR Z,LC;если время вышло, пробуем определить карту как MMC
        LD H,0;HCS в 0
        CALL ACMD41;команда на инициализацию SDC
        JR NZ,AA;ждем cmd. Resp.
        CP 1
        JR Z,AA;ждем выхода из Idle State
        OR A
        JR NZ,LC;если есть какая-либо ошибка, то пробуем определить карту как MMC

;SDv1 Detected
        JR FBS

;-------
LC      LD DE,8000
OO      DEC DE
        LD A,D
        OR E
        JR Z,NOSD;карты нету, либо неизвестный тип
        CALL CMD1;команда на инициализацию MMC
        JR NZ,OO;ждем cmd. Resp.
        CP 1
        JR Z,OO;ждем выхода из Idle State
        OR A
        JR NZ,NOSD;карта не опознана

;MMC Ver.3 Detected
        JR FBS

;-------
SDNEW   LD DE,#01AA
        OR A
        SBC HL,DE
        JR NZ,NOSD;карта не опознана

        LD DE,8000
YY      DEC DE
        LD A,D
        OR E
        JR Z,NOSD
        LD H,#40;HCS в 1
        CALL ACMD41;команда на инициализацию SDC
        JR NZ,YY;ждем cmd. Resp.
        CP 1
        JR Z,YY;ждем выхода из Idle State
        OR A
        JR NZ,NOSD;карта не опознана

;SDv2 Detected
        CALL CMD58;определяем тип адресации (блоковая либо байтовая)
        JR NZ,NOSD
        LD BC,DATA
        IN A,(C)
        IN L,(C)
        IN L,(C)
        IN L,(C)
        BIT 6,A
        JR Z,FBS;SDV2 Byte Addr

;SDv2 Block Address
        LD A,1
SDFND   ;карта успешно опознана
        LD (BLKT),A;выставляем переменную типа адресации (блоковая/байтовая)
;используется для выставления правильного адреса при позиционировании (умножение на 512, если адресация байтовая)
        XOR A
        JP CSH

;-------
FBS     ;выставляем размер блока в 512байт, т. к. карта использует байтовую адресацию
        CALL CMDi6
        JR NZ,NOSD;если не получен cmd. Resp., то считаем, что карты нету
        OR A
        JR Z,SDFND

;-------
NOSD    ;карта не опознана, вылетаем с ошибкой
        CALL SDOFF;выключаем питание (на ZX Evolution не реализовано)
        LD A,1
        OR A
        RET

;-------
CYCL    ;выгреб N байт из SPI
        LD BC,DATA
CY      LD A,#FF
        OUT (C),A
        DEC DE
        LD A,D
        OR E
        JR NZ,CY
        RET


read_sector
        ld a, 1

RDDSE   ;загрузка блока секторов
;        i:[BC,DE] – номер сектора (в BC старшая часть), номер от 0
;               HL – адрес куда грузить
;                A — количество секторов (512б)

        EXA
        CALL CMD18
        ret nz ; JR NZ,$
        EXA

RD1     EXA
        CALL WTDO
        CP #FE
        JR NZ,$-5;ожидаем токен начала данных (%11111110), следом за ним начинается принимаемый сектор (512байт + 2байта crc)

	CALL READS;принимаем 512б данных и пропускаем crc
        EXA
        DEC A
        JR NZ,RD1

        CALL CMD12
        CALL SNB
        CALL WTBY
        call CSH
        xor a: ret

READS   PUSH BC,DE

;читаем непосредственно сам сектор (512б)
        LD BC,DATA
        LD D,32
RZ1
	.16     INI;тираж строки 16 раз
        DEC D
        JP NZ,RZ1

        LD BC,DATA
        IN A,(C);выгребаем 2 байта crc
        IN A,(C)
        POP DE,BC
        RET
;---------------------------------------

SDDSE   ;запись блока секторов
;        i:[BC,DE] – номер сектора
;               HL – адрес в памяти
;                A — количество секторов (512б)

        EXA
        XOR A
        IN A,(CONF)
        AND 2
        RET NZ;если запись запрещена, то ничего не пишем на носитель...

        CALL CMD25;даем команду на много-секторную запись
        CALL WAIT;ожидаем, пока контроллер карты будет готов принять пакет с данными
        EXA
SD1     EXA
        CALL SAVDS;передача блока данных, с токеном начала + 512 байт данных и 2 байт crc
        CALL DRESP;ожидание Data Resp. и снятия busy
        EXA
        DEC A
        JR NZ,SD1

SDND    LD BC,DATA
        LD A,#FD;токен конца много-секторной операции (!)
        OUT (C),A
        CALL SNB
        CALL WTBY
        JP CSH

SAVDS   PUSH BC,DE
        LD BC,DATA
        LD A,#FC;токен начала блока данных
        OUT (C),A

        LD D,32
SV1
	.16     OUTI;тираж строки 16 раз
        DEC D
        JP NZ,SV1

        LD BC,DATA
        LD A,#FF
        OUT (C),A;посылаем 2 байта crc (любой)
        OUT (C),A
        POP DE,BC
        RET

	endmodule
