//	Rocks, sand, grass textures for Geomorph tutorials
//	(c) Patrice St-Gelais 2004
//	Feel free to reuse!


#ifndef (granite_density)
#declare granite_density = 1.2 ;
#end

#ifndef (granite1_density)
#declare granite1_density = 1.0 ;
#end

#ifndef (granite1_grain_scale)
#declare granite1_grain_scale = 1.0 ;
#end

#ifndef (grass_density)
#declare grass_density = 1.0 ;
#end

#ifndef (grass_scale)
#declare grass_scale = 1.0 ;
#end

#ifndef (Transparent)
#declare Transparent = rgbt 1 ;
#end

#ifndef (sand_color)
#declare sand_color = <0.75,0.55,0.35> ;
#end

#ifndef (granite_ambient_finish)
#declare granite_ambient_finish = 0.2 ;
#end

// Sand

#declare wvs = normal {

	gradient x
	slope_map {
		[0    <0.5, 1>]   // start in middle and slope up
		[0.4 <1.0, 0>]   // flat slope at top of wave
		[0.65  <0.5,-1>]   // slope down at mid point
		[0.85 <0.0, 0>]   // flat slope at bottom
		[1    <0.5, 1>]   // finish in middle and slope up
	}
		scale 0.15
		warp { turbulence 0.1 octaves 3 lambda 3.0 }
		scale 1.0/0.15
}

#declare sand1 = texture {
		pigment {colour rgb sand_color}
        normal {
               average
               normal_map {
                          [0.5 wvs 1.0]
                          [0.5 wrinkles 1 scale 0.0025]
                          }
                }
		finish  {
			ambient 0.15
			diffuse 0.8
			}
   }

// Grass: pigment and normal, usable on a plane or on a HF
// p_grass2 gives a wave, n_grass gives the related normal

#declare p_grass1 =
            pigment {
                    granite
                    color_map {
                    [0.0 grass_density*DarkGreen]
                    [0.23 grass_density*DarkGreen]
                    [0.23 grass_density*ForestGreen]
                    [0.66 grass_density*ForestGreen]
                    [1.0 grass_density*LimeGreen]
                    }
                    scale grass_scale * <0.2,0.2,4>
             }
#declare p_grass2 =
            pigment {
                    bumps scale 0.5
                    pigment_map {
                    [0.0 p_grass1 rotate 45*y]
                    [0.3 p_grass1 rotate 22*y]
                    [0.6 p_grass1]
                    [1.0 p_grass1 rotate -30*y]
                    }
  	            warp { turbulence 0.1 }
            }
 
#declare g1 = grass_density*<0.54, 0.6, 0.23>;
#declare g2 = grass_density*<0.29, 0.48, 0.13>;
#declare g3 = grass_density*<0.45, 0.63, 0.1>;
#declare g4 = grass_density*<0.4, 0.5, 0.2>;

#declare p_grass =
            pigment {
                   bumps scale 0.5
                   pigment_map {
                   [0.0 granite color_map {[0.3 color g1] [0.6 color g2]} scale grass_scale*<0.05,0.05,0.25> rotate 45*y]
                   [0.3 granite color_map {[0.3 color g2] [0.6 color g3]} scale grass_scale*<0.05,0.05,0.25> rotate 22*y]
                   [0.6 granite color_map {[0.3 color g3] [0.6 color g4]} scale grass_scale*<0.05,0.05,0.25> ]
                   [1.0 granite color_map {[0.3 color g4] [0.6 color g1]} scale grass_scale*<0.05,0.05,0.25> rotate -30*y]
                   }
	           warp { turbulence 0.1 }
            }
#declare t_grass =
 texture {pigment {p_grass} normal {pigment_pattern{p_grass} 1.0}}
 
// Pink granite with veins (cracks)
#declare granite_1_grain_pigment = 
	pigment {
		granite
		color_map {
		[0.0 rgb granite1_density*0.1*<0.9,0.73,0.65>]
		[0.0 rgb granite1_density*0.3*<0.9,0.73,0.65>]
		[0.6 rgb granite1_density*<0.9,0.73,0.65> ]
		[0.7 rgb granite1_density*<0.9,0.73,0.65> ]
		[1.0 rgb <0,0,0>]
		
		}
		scale granite1_grain_scale*0.02
	}
	
#declare granite_1_pigment =
        pigment {
                    crackle
                    pigment_map {
                    [0.0 rgb <0.4,0.23,0.15>]
                    [0.035 rgb <0.7,0.6,0.55>]
                    [0.05 rgbt <0.9,0.73,0.65,1.0>]
                    [1.0 Transparent ] //rgb <1,0.83,0.75>]
                    }
                    warp { turbulence 0.2 }
            }
	    
#declare granite_1 = 
texture {pigment {granite_1_pigment}
	pigment {granite_1_grain_pigment}
	normal {pigment_pattern {granite_1_pigment} 1}
        scale 0.5}

// Usual pink granite (grainy texture)
#declare granite_2 =
        pigment {
                    wrinkles
                    pigment_map {
                    [0.0 rgb granite_density* 0.2*<0.4,0.23,0.15>]
                    [0.05 rgb  granite_density*0.4*<0.7,0.7,0.55>]
                    [0.2 rgb  granite_density*0.6*<0.9,0.73,0.65>]
                    [1.0 rgb  granite_density*0.8*<1,0.83,0.75>]
                    }
                    warp { turbulence 0.5 }
		    scale 0.2
            }

// Gradient for mountains

#declare grad_mount1 =
 texture {
    gradient y
    texture_map {
      [0.0  granite_1 scale 0.2]
      [0.1  granite_1 scale 0.2]
      [0.11  t_grass scale 0.1]
      [1.0  t_grass scale 0.1]
    }
	warp { turbulence 0.01 omega 0.8 lambda 3.0 }
	translate y*-0.05
  }


// Gradient for shore rocks, with an underwater part
// We stack 4 textures:
// 1. A pink granite
// 2. A band of algae
// 3. Grass on the top of the rocks
// 4. A mask for darkening the wet rocks and giving some bumps
// Then we add on the top a gradient finish, for a shiny wet part

#declare shore_granite_base =
 // 1. Granite

 // texture { pigment {rgb <0.9,0.73,0.65>} // Flat granite pink

 texture { pigment { granite_2 scale 0.05 } 
      finish {ambient granite_ambient_finish} }

  // 2. Grass on the top
 texture {
      pigment {
            gradient y
            pigment_map {
              [0.0  Transparent]
              [0.4  Transparent]
	      [0.45 color rgb  grass_density*<0.2,0.2,0.1> ] // Some humus
              [0.6  p_grass2 ]
              [1.0  p_grass2 ] 
            }
	  
   	  warp { turbulence 0.1
           omega 0.8 	// How large is the step between octaves, default 0.5
  	   lambda 3		// How large is the random move, default 2.0
	   }
      }
      finish {ambient granite_ambient_finish}
  }
/*
// 4. A band of algae over the wet and shiny line
 texture {
      pigment {
            gradient y
            pigment_map {
              [0.0  Transparent]
              [0.2  color grass_density*ForestGreen transmit 0.4 ]
              [0.3  Transparent]
            }
      }
      finish {ambient granite_ambient_finish}
  }
*/
  // 5. Darkening the wet rocks
   texture {
	pigment {
                gradient y
                pigment_map {
			[0.0 color rgb  granite_density*<0,0,0.05> transmit 0.3]
                        [0.22 color rgb  granite_density*<0,0,0.05> transmit 0.5]
                        [0.3 color rgb <0,0,0> transmit 1]
                        [1 color rgb <0,0,0> transmit 1]
                }
	}
  }

  
// end shore_granite_base


// Add some specular light (it's wet!)
// and a band of algae

#declare algae_light =
	texture {pigment { color rgb grass_density*1.2*<0.5,0.5,0.25> }   
      		finish {ambient granite_ambient_finish}}
#declare algae_medium =
	texture {pigment { color rgb grass_density*1.2* <0.32,0.4,0.2> }  
      		finish {ambient granite_ambient_finish} }
#declare algae_dark =
	texture {pigment { color rgb grass_density*1.2*<0.15,0.3,0.15> }  
      		finish {ambient granite_ambient_finish} }

#declare shore_granite =
      texture {
                gradient y
                texture_map {
                        [0.0 shore_granite_base  ]
                        [0.16 shore_granite_base
 				finish {phong 0.2 phong_size 50} ]
                        [0.17 algae_light finish {phong 0}  ]
                        [0.175 algae_medium finish {phong 0}  ]
                        [0.195 algae_dark finish {phong 0}  ]
                        [0.22 shore_granite_base
			// phong = 0.0 to 1.0, 0.0 = no highlight, 1.0 = complete saturation with the light color
			// phong_size = 1.0 to 250.0; high value = small highlight = shinier
 				finish {phong 0.4 phong_size 25} ]
                        [0.25 shore_granite_base ]
                        [1 shore_granite_base  ]
                }
		translate y*-0.05 // Avoid some cycle
 	warp { turbulence 0.03
           omega 0.8 	// How large is the step between octaves, default 0.5 - + gives "mixed up" bands
  	   lambda 3		// How large is the random move, default 2.0 - ++ gives a "granular" effect
	   }
	   }
