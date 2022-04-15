#define _LARGEFILE64_SOURCE

#include <stdio.h>
#include <getopt.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <strings.h>
#include <string.h>

#define BUFFER_SIZE (1024*1024*4)

static char *name = "split-%02d.raw";

loff_t max (loff_t a, loff_t b) {
  return (a > b? a: b);
}

loff_t min (loff_t a, loff_t b) {
  return (a < b? a: b);
}

void sp_write (FILE *fd, void *buf, int count) {
  int l, start = 0, block;

  while (count > 0) {
    block = BUFFER_SIZE;
    if (block > count) 
      block = count;
    if ((l = fwrite ((char*)(buf + start), 1, block, fd)) < 0) {
      perror ("bsplit write");
      printf ("%p %x\n", buf, count);
      exit (-1);
    }
    start += l;
    count -= l;
  }
}						   


void bsplit (char *file, char *split_spec) {
  char *ibuf, *obuf;
  struct stat64 statbuf;
  loff_t size = 0, skip_length;
  int in = 0;
  FILE *fin, *fout = NULL;
  loff_t pos = 0, opos = 0, i = 0, ipos = 0, read_len = 0;
  int file_part = 1;
  char ofilename[1024];
  loff_t positions[1024];
  char *split;
  
  if (! (ibuf = (char *) malloc (BUFFER_SIZE))) {
    perror ("bsplit");
    exit (-1);
  }
      
  if (! (obuf = (char *) malloc (BUFFER_SIZE))) {
    perror ("bsplit");
    exit (-1);
  }
  
  if (! (in = open64(file, O_RDONLY))) {
    perror ("bsplit open");
    exit (-1);
  }

  if (fstat64 (in, &statbuf) < 0) {
    perror ("bsplit stat");
    exit (-1);
  }

  size = statbuf.st_size;

  fin = fdopen(in, "r");
  
  positions[i++] = 0;
  split = (char*)strtok(split_spec, ":");
  positions[i++] = atoll(split);
  while ((split = strtok(NULL, ":")) != NULL) 
    positions[i++] = atoll(split);

  positions[i++] = size;
  
  i = 0;
  
  while (pos < size) {
    if (pos == positions[i]) {
      if (fout) {
	sp_write(fout, obuf, opos);
	fclose(fout);
      }
      sprintf(ofilename, name, file_part++);
      fprintf(stderr, "Opening %s\n", ofilename);
      if ((fout = fopen64(ofilename, "w")) == NULL) {
	perror("bsplit");
	exit(0);
      }
      i++;
      opos = 0;
    }

    if (ipos == read_len) {
      read_len = fread(ibuf, 1, min(BUFFER_SIZE, size - pos), fin);
      ipos = 0;
    }

    if (opos == BUFFER_SIZE) {
      sp_write(fout, obuf, BUFFER_SIZE);
      opos = 0;
    }

    skip_length = min(min(read_len - ipos, BUFFER_SIZE - opos),
		      positions[i] - pos);

    memcpy(obuf + opos, ibuf + ipos, skip_length);
    opos += skip_length;
    ipos += skip_length;
    pos += skip_length;
  }

  printf("%llx %llx\n", pos, size);

  if (fout) {
    sp_write(fout, obuf, opos);
    fclose(fout);
  }
    
}

      
int main (int argc, char **argv) {
  int c;
  char *file, *split_spec;
  
  while (1) {
    int option_index = 0;
    static struct option long_options[] = {
      {"help", 1, 0, 'h'},
      {0, 0, 0, 0}
    };

    c = getopt_long (argc, argv, "hn:", long_options, &option_index);
    if (c == -1)
      break;

    switch (c) {
    case 'h':
      printf ("\
Usage: bsplit [--name <format>] <file> <split-spec>\n");
      break;

    case 'n':
      name = (char*) malloc(strlen(optarg) + 1);
      strcpy(name, optarg);
      break;

    }
  }

  if (! argv[optind]) {
    printf ("Usage: bsplit <file>\n");
    exit (-1);
  } else {
    file = argv[optind++];
    if (! argv[optind]) {
      printf ("Usage: bsplit <file>\n");
      exit (-1);
    } else {
      split_spec = (char*) malloc(strlen(argv[optind]) + 1);
      strcpy(split_spec, argv[optind]);
    }
  }

  bsplit (file, split_spec);
  
  exit (0);
}
  
