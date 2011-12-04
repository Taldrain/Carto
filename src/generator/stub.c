/* Stub to perlin noise */

# include "perlin.h"
# include "bmp.h"

#include <stdlib.h>
#include <time.h>

# include <caml/mlvalues.h>
# include <caml/alloc.h>
# include <caml/memory.h>
# include <caml/fail.h>
# include <caml/callback.h>
# include <caml/custom.h>
# include <caml/intext.h>
# include <caml/bigarray.h>


CAMLprim value
ext_init_random(value cseed){
  unsigned int seed;
  seed = Long_val(cseed);
  if (seed)
    srandom(seed);
  else
   srandom(time(NULL));
  return Val_unit;
}

CAMLprim value
ext_perlin(value cx, value cy, value calpha, value cbeta, value cn){
  double x, y, alpha, beta, perl ;
  int n;

  x = Double_val(cx) ;
  y = Double_val(cy) ;
  alpha = Double_val(calpha) ;
  beta = Double_val(cbeta) ;
  n = Long_val(cn) ;
  perl = PerlinNoise2D(x,y,alpha,beta,n);
  return (copy_double(perl));
}

void *flatten_image(unsigned char *image, int w, int h){
  unsigned char *img = NULL;
  int i,j,l,pad,k=0;
  img = malloc(align4(w*3) * h);
  memset(img,0,align4(w*3) * h);
  pad = align4(w*3) - w*3;
  for (j=0; j<h; j++) {
    for (i=0; i<w; i++) {
      for (l=0; l<3; l++)
	img[k+(2-l)] = image[i*w*3 + j*3 + l];
      k += 3;
    }
    if (pad)
      k += pad;
  }

  return img;
}

CAMLprim value
ext_save_bmp(value cimage){
  void *img = NULL;
  struct bmp_image *bmp;
  int w,h;
  w = Bigarray_val(cimage)->dim[0];
  h = Bigarray_val(cimage)->dim[1];

  img = flatten_image(Data_bigarray_val(cimage),w,h);
  bmp = make_bmp(img,w,h);
  save_bmp(bmp,STDOUT_FILENO);
  free_image(bmp);

  return Val_unit;
}
