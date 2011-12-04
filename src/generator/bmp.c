/* BMP basic manipulation */

# include "bmp.h"

void *copy_image(struct bmp_image *bmp){
  struct bmp_image  *new;

  new            = malloc(sizeof(struct bmp_image));
  new -> header  = malloc(sizeof(struct bmp_header));
  new -> info    = malloc(sizeof(struct bmp_info));
  new -> image   = malloc(bmp->size);

  new -> size    = bmp -> size;
  new -> mmaped  = 0;

  memcpy(new->header, bmp->header, sizeof(struct bmp_header));
  memcpy(new->info, bmp->info, sizeof(struct bmp_info));
  memcpy(new->image, bmp->image, bmp->size);

  return new;
}

void free_image(struct bmp_image *bmp){
  free(bmp -> header);
  free(bmp -> info);

  if (bmp -> mmaped)
    munmap(bmp -> image, bmp -> size);
  else
    free(bmp -> image);

  free(bmp);
}
 
int save_data(char *buf, int size, int fd){
  int res=0,index=0;

  while (index < size){
    res = write(fd, buf + index, size - (index));
    if (res == -1){
      if (errno != EINTR)
        return -1 ;
    } else
      index += res;
  }

  return index;

}

int save_bmp(struct bmp_image *bmp, int fd){
  int saved=0;

  saved += save_data((char*)(bmp -> header)+2, 14, fd);
  saved += save_data((char*)(bmp->info), sizeof(struct bmp_info), fd);
  saved += save_data(bmp -> image, bmp -> size, fd);
  return saved;
}

struct bmp_image *make_bmp(void *image, int w, int h){
  struct bmp_image *new;
  new            = malloc(sizeof(struct bmp_image));
  new -> header  = malloc(sizeof(struct bmp_header));
  new -> info    = malloc(sizeof(struct bmp_info));
  new -> image   = image ;
  new -> size    = align4(w*3) * h;
  new -> mmaped  = 0;

  new -> header -> id[0] = 'B';
  new -> header -> id[1] = 'M';
  new -> header -> file_size = 54 + new -> size;
  new -> header -> reserved = 4242;
  new -> header -> offset = 54;

  new -> info -> header_size = 40;
  new -> info -> width = w;
  new -> info -> height = h;
  new -> info -> planes = 1;
  new -> info -> bit = 24;
  new -> info -> comp_method = 0;
  new -> info -> image_size = new -> size;
  new -> info -> hres = new -> info -> vres = 2835;
  new -> info -> color_number = new -> info -> important_color = 0;

  return(new);
}
