;�mer Faruk B�T�K��O�LU
;Numara: 161044010
;A0 haf�zas�na b�l�necek olan say�y� hexadecimal olarak giriniz.
;B0 haf�zas�na b�len say�y� hexadecimal olarak giriniz.
;Kalan (mod i�leminin sonucu) ekranda ve C0 haf�zas�nda g�z�kecektir.

load R0,[0xA0] ;b�l�nen
load R1,[0xB0] ;b�len
load R2,0d
load R3,0d ;kalan s�f�r
load R4,48d
load R5,1d ;genel kullan�m i�in 1 de�eri
load R6,0d ;kalan bulma

jmpEQ R1=R0,tambolunur
jmpLE R1<=R0,topla
store R1,[0xC0] ;sonu� girilen de�erin kendisidir

kontrol:
jmpEQ R2=R0,tambolunur
jmpLE R2<=R0,topladevam
jmp birbir

kontroll:
jmpEQ R7=R0,kalanbul
jmpLE R7<=R0,birbir

topla:
addi R2,R1,R1
move R7,R1
jmp kontrol

topladevam:
move R7,R2
addi R2,R2,R1
jmp kontrol

tambolunur:
store R3,[0xC0] ;sonu� 0'd�r
load RF,48d
halt

birbir:
addi R7,R7,R5
addi R6,R6,R5
jmp kontroll

kalanbul:
store R6,[0xC0]
addi R6,R6,R4
move RF,R6
halt

