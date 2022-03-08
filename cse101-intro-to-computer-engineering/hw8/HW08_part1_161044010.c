#include <stdio.h>
/*Faktoriyel, üs alma ve sinüs hesaplama işlemleri için fonksiyon tanımladım*/
int fac (int);
double usAl (double, int);
double sinusHesapla (double);
int main()
{
	double derece,radyan,sonuc,pi=3.1415926535;
	printf("Hangi acinin sinusu hesaplansin? \n");
	scanf("%lf",&derece);
	
	/*Girilen dereceli değeri radyan değerine çevirme*/
	radyan = derece*pi/180;

	/*Sinüsü hesaplatma ve ekrana yazdırma*/
	sonuc = sinusHesapla(radyan);
	printf("Sonuc = %f\n",sonuc);
}

/*Faktoriyel hesaplama fonksiyonu*/
int fac (int n)
{
	int i, sonuc = 1;
	for(i = 1; i <= n; i++) 
	{
		sonuc *= i;
	}
	return sonuc;
}

/*Üs hesaplama fonksiyonu*/
double usAl (double x, int n)
{
	int i;
	double sonuc=1;
	
	/*Eğer üssümüz 0 değilse üs alma işlemi yapsın. 0'sa sonuc değişkeninin değeri olan 1'i döndürür.*/
	if(n != 0)
	{
		for(i = 1; i <= n; i++)
		{
			sonuc = sonuc * x;
		}
	}
	
	return sonuc;
	
}

/*Sinüs hesaplama fonksiyonu*/
double sinusHesapla (double x)
{
	int i,isaret,terim;
	double sonuc=0;
	
	printf("Taylor aciliminda kac terim kullanilsin? \n");
	scanf("%d",&terim);
	
	for(i = 0; i < terim; i++)
	{
		/*Taylor açılımındaki + ve - işaretlerini -1'i üs alma işlemine sokarak elde ettim*/
		isaret = usAl(-1,i);

		/*Taylor açılımındaki işlemleri sırayla yapıp sonuca ekle*/
		sonuc = sonuc + isaret * usAl(x,2*i+1) / fac(2*i+1);
	}
	
	
	return sonuc;
}

















