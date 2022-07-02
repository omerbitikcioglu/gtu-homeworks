#include <stdio.h>
#include <stdlib.h>
#include <time.h>

void rastgeleBoy(int cikoBoylar[], int elemanSayisi);
void ekranaYazdir(int cikoBoylar[], int elemanSayisi);
void yenenSay(int cikoBoylar[], int elemanSayisi);
void kimNeYedi(int meh, int ays);
void kazananKim(int meh, int ays);

int main()
{
	int elemanSayisi, cikoBoylar[6];
	
	/*Çikolata boyları dizisinin eleman sayısını bulur*/
	elemanSayisi = sizeof(cikoBoylar)/sizeof(cikoBoylar[0]);
	
	rastgeleBoy(cikoBoylar, elemanSayisi);
}

/*Rastgele boy değerleriyle bir dizi oluşturma*/
void rastgeleBoy(int cikoBoylar[], int elemanSayisi)
{
	int i,a;
	srand(time(NULL));
	
	for(i=0; i < elemanSayisi; i++)
	{
	/* a'ya [1-11] kapalı aralığında değerler atama
	a'ya 0 gelme ihtimali olduğundan kontrol ifadesi koydum */
		do {
			a = rand()%12;
		} while(a == 0);
		
		cikoBoylar[i] = a;
	}

	ekranaYazdir(cikoBoylar, elemanSayisi);
	yenenSay(cikoBoylar,elemanSayisi);
}

/*Rastgele boylardan oluşan diziyi ekrana yazdırma*/
void ekranaYazdir(int cikoBoylar[], int elemanSayisi)
{
	int i; 
	
	for(i=0; i < elemanSayisi; i++)
	{
		printf("%d ",cikoBoylar[i]);
		
		/*Dizinin son değerini yazdıktan sonra 1 satır aşağı insin*/
		if(i == elemanSayisi-1)
			printf("\n");
	}
}

/*Mehmet ve Ayşe'nin kaçar tane çikolata yediğini hesaplar*/
void yenenSay(int cikoBoylar[], int elemanSayisi)
{
	int i=0, j=elemanSayisi-1, meh=0, ays=0, t_meh, t_ays, a_kalan=0, m_kalan=0;
	
	/*Yenecek çikolata kalmayana kadar döngü devam eder*/
	for( ; elemanSayisi != 0; )
	{
		/*Geriye sadece 1 çikolata kalırsa...
			İkisi de aynı çikolataya göz dikerse*/
		if(i == j)
		{
			/*Ayşe çikolataya hiç dokunmamışsa çikolatayı Mehmet yer.
				Ayşe yemeye başladıysa çikolatayı Ayşe yer*/
			if(t_ays == cikoBoylar[j]*2)
			{
				meh++;
				elemanSayisi -= 1;
			}
			else
			{
				ays++;
				elemanSayisi -= 1;
			}
		}
		else
		{
			/*Yenme sürelerinin hesaplanması*/
			if(a_kalan == 0)
				t_ays = cikoBoylar[j]*2;
			else
				t_ays = a_kalan;
		
			if(m_kalan == 0)
				t_meh = cikoBoylar[i];
			else
				t_meh = m_kalan;
		
			/*Kimin yeme süresi daha kısaysa o kişi çikolatasını daha önce bitirir
				Süreler eşitse aynı anda bitirirler*/
			if(t_meh < t_ays)
			{
				while(t_meh != 0)
				{
					t_meh--;
					t_ays--;
				}
				meh++;
				i++;
				a_kalan += t_ays;
				elemanSayisi -= 1;
			}
			else if(t_ays < t_meh)
			{
				while(t_ays != 0)
				{
					t_meh--;
					t_ays--;
				}
				ays++;
				j--;
				m_kalan += t_meh;
				elemanSayisi -= 1;
			}
			else
			{
				meh++;
				ays++;
				i++;
				j--;
				elemanSayisi -= 2;
			}
		}
	}

	kimNeYedi(meh,ays);
	kazananKim(meh,ays);
}

void kimNeYedi(int meh, int ays)
{
	/*Mehmet ve Ayşe'nin yediği çikolata sayılarını ekrana yazdırma*/
	printf("Mehmet: %d\n", meh);
	printf("Ayse: %d\n\n", ays);
}

void kazananKim(int meh, int ays)
{
	/*Kim daha çok çikolata yediyse o kazanır
		Eşit sayıda yemişlerse durum berabere*/
	if(meh > ays)
		printf("Kazanan: Mehmet");
	else if(ays > meh)
		printf("Kazanan: Ayse");
	else
		printf("Kazanan: Berabere");
}