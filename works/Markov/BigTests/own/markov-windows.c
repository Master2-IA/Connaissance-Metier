/* Copyright (C) 1999 Lucent Technologies */
/* From 'Programming Pearls' by Jon Bentley */

/* markov.c -- generate random text from input document
	Usage: markov k m  <text.in  >text.out 

        k: number of words per phrase
        m: number of words to print  

	History
	  Original from 'Programming Pearls' by Jon Bentley 
	  Modified 6 May 2005 by ccm 
		Change k and m from compile time to command line inputs 
		Change output from one word per line to many per line 
	  Modified 15 August 2005 ccm 
	         Add srand to initialize rng with system time 
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <windows.h>
#include <time.h>

char inputchars[4500000];
char *word[800000];
int nword = 0, linelen = 0, k;
int m;
unsigned long int nbCall[3];


/* word comparison */ 
int wordncmp(char *p, char* q)
{	int n = k;
	for ( ; *p == *q; p++, q++)
		if (*p == 0 && --n == 0)
			return 0;
	return *p - *q;
}

/* called by system qsort */ 
int sortcmp(const void * p, const void * q)
{
  char ** p1 = (char**) p;
  char ** q1 = (char**) q;
  nbCall[2]++;
  return wordncmp(*p1, *q1);
}

/* skip over words in text */ 
char *skip(char *p, int n)
{	for ( ; n > 0; p++)
		if (*p == 0)
			n--;
	return p;
}

/* print a word */ 
void writeword(char *s)
{	int len = strlen(s);
	if (linelen + len > 70) {
		printf("\n"); 
		linelen = 0;
	} else {
		printf(" ");
	}
	linelen += len + 1;
	printf("%s", s);
}


/**
 * Returns the amount of CPU time used by the current process,
 * in seconds, or -1.0 if an error occurred.
 */
WORD getCPUTime( )
{
  FILETIME createTime;
  FILETIME exitTime;
  FILETIME kernelTime;
  FILETIME userTime;
  if ( GetProcessTimes( GetCurrentProcess( ),
                        &createTime, &exitTime, &kernelTime, &userTime ) != -1 )
    {
      SYSTEMTIME userSystemTime;
      if ( FileTimeToSystemTime( &userTime, &userSystemTime ) != -1 )
        return userSystemTime.wMilliseconds;
    }
  return -1;		/* Failed. */
}

int main(int argc, char* argv[])
{	int i, wordsleft, lo, mid, up;
	char *phrase, *p;
	LARGE_INTEGER startTime, endTime;
	
	for(i = 0; i < 3; i++)
		nbCall[i] = 0;
if(atoi(argv[3]) == 0)
{
  fprintf(stdout, "\"k\";");
  fprintf(stdout, "\"m\";");
  fprintf(stdout, "\"n\";");
  fprintf(stdout, "\"count1\";");
  fprintf(stdout, "\"count2\";");
  fprintf(stdout, "\"count3\";");
  fprintf(stdout, "\"time\"\n");
}
        //startTime = getCPUTime( );
QueryPerformanceCounter((LARGE_INTEGER *)&startTime);

	k = atoi(argv[1]);
	m = atoi(argv[2]);
	wordsleft = m;
	
  unsigned __int64 ttt;
QueryPerformanceCounter((LARGE_INTEGER *)&ttt);
  srand(ttt);
        //srand((unsigned) time(0));  
	
	word[0] = inputchars;
	while (scanf("%s", word[nword]) != EOF) {
          /* if(nword <= 50) printf("%s:", word[nword]); */
          word[nword+1] = word[nword] + strlen(word[nword]) + 1;
          nword++;
	}
	for (i = 0; i < k; i++)
		word[nword][i] = 0;/*
	for (i = 0; i < k; i++)
		printf("%s\n", word[i]);*/
	qsort(word, nword, sizeof(word[0]), sortcmp);
	phrase = inputchars;
	for ( ; wordsleft > 0; wordsleft--) {
		lo = -1;
		up = nword;
		while (lo+1 != up) {
			mid = (lo + up) / 2;
		nbCall[0]++;
			if (wordncmp(word[mid], phrase) < 0)
				lo = mid;
			else
				up = mid;
		}
		nbCall[1]++;
		for (i = 0; wordncmp(phrase, word[up+i]) == 0; i++)
		{
			if (rand() % (i+1) == 0)
				p = word[up+i];
			nbCall[1]++;
		}
		phrase = skip(p, 1);
		if (strlen(skip(phrase, k-1)) == 0)
			break;
		//writeword(skip(phrase, k-1));
	}
        //printf("\n");
QueryPerformanceCounter((LARGE_INTEGER *)&endTime);
        //endTime = getCPUTime( );
		
		LARGE_INTEGER freq;
		QueryPerformanceFrequency((LARGE_INTEGER *)&freq);

        //fprintf( stderr, "CPU time used = %lf\n", (endTime - startTime) );
  fprintf(stdout, "%i;%i;%i;%lu;%lu;%lu;%f\n", k, m, nword, nbCall[2], nbCall[0], nbCall[1], (endTime.QuadPart - startTime.QuadPart) / (double)freq.QuadPart);
  //fprintf(stderr, " [%lu %lu]\n", endTime, startTime);
	exit(EXIT_SUCCESS);
}



