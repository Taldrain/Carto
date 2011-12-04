/* BMP basic manipulation */

# ifndef ___BMP_H
# define ___BMP_H

# include <stdlib.h>
# include <fcntl.h>
# include <unistd.h>
# include <errno.h>
# include <stdio.h>
# include <sys/mman.h>
# include <string.h>

# define align4(i) ((i%4)?((i/4)+1)*4:i)

struct bmp_header {
  short int padding;
  char         id[2];
  unsigned int file_size;
  unsigned int reserved;
  unsigned int offset;
};

struct bmp_info {
  unsigned int header_size;
  unsigned int width;
  unsigned int height;
  short int    planes;
  short int    bit;
  unsigned int comp_method;
  unsigned int image_size;
  int hres;
  int vres;
  unsigned int color_number;
  unsigned int important_color;
};

struct bmp_image {
  struct bmp_header *header;
  struct bmp_info   *info;
  unsigned int       size;
  void              *image;
  char               mmaped;
};

/* Base manipulations */
void *copy_image(struct bmp_image *bmp);
void free_image(struct bmp_image *bmp);
struct bmp_image *make_bmp(void *image, int w, int h);

/* Writing */
int save_data(char *buf, int size, int fd);

/* Saving */
int save_bmp(struct bmp_image *bmp, int fd);


# endif
