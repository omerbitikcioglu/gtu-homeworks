;Ömer Faruk BÝTÝKÇÝOÐLU
;Numara: 161044010
;A0 hafýzasýna bölünecek olan sayýyý hexadecimal olarak giriniz.
;B0 hafýzasýna bölen sayýyý hexadecimal olarak giriniz.
;Kalan (mod iþleminin sonucu) ekranda ve C0 hafýzasýnda gözükecektir.

load R0,[0xA0] ;bölünen
load R1,[0xB0] ;bölen
load R2,0d
load R3,0d ;kalan sýfýr
load R4,48d
load R5,1d ;genel kullaným için 1 deðeri
load R6,0d ;kalan bulma

jmpEQ R1=R0,tambolunur
jmpLE R1<=R0,topla
store R1,[0xC0] ;sonuç girilen deðerin kendisidir

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
store R3,[0xC0] ;sonuç 0'dýr
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

