/*Gerekli kütüphaneleri çağırdım*/
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <math.h>
int main()
{
	/*Gerekli değişken ve dizileri tanımladım*/
	int r, i, v1_L0 = 0, v2_L0 = 0, sum1 = 0, sum2 = 0, vektor1[20], vektor2[20];
	
	double sim, dot_product=0, norm1, norm2;
	/*İşlemciden rastgele sayılar çağırdım*/
	srand(time(NULL));
	
	/*İlk dizinin elemanlarını 0,10 arası tamsayılar ile doldurdum.*/
	for(i = 0; i < 20; i++)
	{
		r = rand()%11;
		vektor1[i]=r;
	}
	
	/*İkinci dizinin elemanlarını 0,10 arası tamsayılar ile doldurdum.*/
	for(i = 0; i < 20; i++)
	{
		r = rand()%11;
		vektor2[i]=r;
	}
	
	/*Eleman 0'dan farklıysa L0 normları 1 artıyor*/
	for(i = 0; i < 20; i++)
	{
		if(vektor1[i] != 0)
			v1_L0 ++;
		if(vektor2[i] != 0)
			v2_L0 ++;
	}
	
	/*Vektörlerin noktasal çarpımı*/
	for(i = 0; i < 20; i++)
	{
		dot_product += vektor1[i]*vektor2[i];
	}
	
	/*Vektörlerin normları*/
	for(i = 0; i < 20; i++)
	{
		sum1 += pow(vektor1[i],2);
		sum2 += pow(vektor2[i],2);	
	}
	norm1 = sqrt(sum1);
	norm2 = sqrt(sum2);
	
	/*Cosinus benzerliği*/
	sim = dot_product/(norm1*norm2);
	
	/*Vektörleri ekrana yazdırma*/
	printf("\nVektorler: \n");
	printf("v1 : ");
	for(i = 0; i < 20; i++)
	{
		printf("%d",vektor1[i]);
		if(i != 19)
			printf(",");
	}
	printf("\nv1 : ");
	for(i = 0; i < 20; i++)
	{
		printf("%d",vektor2[i]);
		
		if(i != 19)
			printf(",");
	}
	printf("\n\n");
	
	/*Vektörlerin L0 normlarını ekrana yazdırma*/
	printf("v1 L0 norm : %d\n",v1_L0);
	printf("v2 L0 norm : %d\n",v2_L0);
	
	printf("\n");
	
	/*Vektörlerin toplamını ekrana yazdırma*/
	printf("v1 + v2 = ");
	for(i = 0; i < 20; i++)
	{
		printf("%d",vektor1[i]+vektor2[i]);
		
		if(i != 19)
			printf(",");
	}
	
	printf("\n\n");
	
	/*Cosinus benzerliğini ekrana yazdırma*/
	printf("sim(v1,v2) : %f\n\n", sim);	
}
