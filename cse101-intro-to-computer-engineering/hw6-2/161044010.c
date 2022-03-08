#include <stdio.h>

int main() {

	/*input değişkeni kullanıcının menüde işlem yapmak için seçtiği değer,
	loop ise menünün tekrar gözüküp gözükmemesi işlemini kontrol eden değişkendir.*/

	/*Başlangıçta menünün gözükmesi için loop'a ilk olarak 1 değerini atadık*/	
	int input, loop=1;

	/*loop değeri 1 olduğunda menüyü göstersin.*/
	while(loop==1) {

		/*Menüyü yazdır*/
		printf("\n--- MENU ---\n");
		printf("1. Yıldızlardan elmas ciz\n");
		printf("2. Ogrenci bilgisini goster\n");
		printf("0. Cikis\n");
		printf("Seçeneğiniz: ");
	
		/*Kullanıcıdan alınan değeri input değişkenine at. Menünün tekrar gözükmemesi için loop'a 0 atadık*/
		scanf("%d", &input);
		loop=0;

		/*Girilen değer 1 ise elmas çizdirme kodları çalışsın*/
		if(input==1) {

			/*Gerekli değişkenleri tanımlama*/
			int h, line, space, star, halfh;
	
			/*Yükseklik girdisini kullanıcıdan alma*/
			printf("Elmasın yüksekliğini giriniz...\n");
			scanf("%d", &h);

			/*Yarı yükseklik değeri*/
			halfh= (h+1)/2;

			/*Yarı yüksekliğe kadar düz piramit çizdiriyorum*/
			for(line=1; line <= halfh; line++) {

			 	/*Yükseklikle bulunulan satırın farkı kadar boşluk bırakıyorum*/
				for(space=1; space <= halfh-line; space++){

					printf(" ");			

				}	
		
				/*Bulunulan satırın 2 katının 1 eksiği kadar yıldız yazdırıyorum*/
				for(star=1; star <= 2*line-1; star++) {

					printf("*");

				}
		
			/*Satır sonlandıktan sonra bir alt satıra geçiyorum*/
			printf("\n");

			}

			/* Diğer yarı yüksekliğin 1 eksiğinden başlayarak ters piramit çizdiriyorum.
			Çünkü düz piramit çizerken tabanı yazdırmıştım. Tekrar yazdırmak istemiyorum.
	
			Bu sefer piramiti tersten yazdığımız için bulunulan satırı arttırmak yerine tek tek
			azaltıyoruz. Sanki sondan başa doğru düz bir piramit çizmişiz gibi... */
			for(line=halfh-1; line >= 1; line--) {

				/*Yükseklikle bulunulan satırın farkı kadar boşluk bırakıyorum*/
				for(space=1; space <= halfh-line; space++){

					printf(" ");
		
				}

				/*Bulunulan satırın 2 katının 1 eksiği kadar yıldız yazdırıyorum*/
				for(star=1; star <= 2*line-1; star++){

					printf("*");
	
				}

			/*Satır sonlandıktan sonra bir alt satıra geçiyorum*/
			printf("\n");

		}
		
		/*Menünün tekrar gözükmesi için loop değerini tekrar 1 olarak atadık*/
		loop=1;

		}
	
		/*Girilen değer 2 ise öğrenci bilgileri gözüksün*/
		else if(input==2) {

			printf("\nAd: Ömer Faruk\n");
			printf("Soyad: Bitikçioğlu\n");
			printf("Ogrenci No: 161044010\n");
		
			loop=1;
		
		}
		
		/*Girilen değer 0 ise loop'a 0 değerini atadık bu sayede döngü sonlandı ve menü tekrar gözükmedi*/
		else if(input==0) {

			loop=0;

		}
	
		/*Girilen değer 1,2 veya 0 değilse bunlar dışında menüde olmayan bir değer girilmiş demektir*/
		else {

			/*Geçersiz değer girildiğini yazdırdık.*/
			printf("\nGecersiz deger girdiniz.\n");
			loop=1;

		}
	
	
	}	

	
return 0;

}


