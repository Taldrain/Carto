#declare gl_angle = 28;
#declare aspect_ratio =  1.3333300;
#declare tr_x = -0.0000000;
#declare tr_y =  0.0700000;
#declare tr_z = -0.9250000;
#declare rot_x = 11;
#declare rot_y = 0;
#declare rot_z = 0;

#declare rock_color = 0.5*(<0.72,0.66,0.4>) ;
#declare rock_turbulence=0.2;
#declare rock_scale=0.05;
#declare rock_normal=1.5;
#declare sky_density=0.6;
#declare forest_color_density=0.8;
#declare water_density=0.35;
#declare fog_distance=10;
#declare fog_transparency=0.1;
#declare fog_altitude=0.15;

// Contrôle de la neige
#declare snow_density=1.0;
#declare snow_color=<0.95, 0.95, 1.0>;
#declare snow_height=0.5;
// Contrôle de la forêt
#declare forest_color_density=1.0;
#declare forest_normal=1.0;
#declare forest_normal_scale=1.0;
// Contrôle des arbustes
#declare bushes_color_density=1.0;
#declare bushes_normal=1.0;
#declare bushes_normal_scale=1.0;
#declare UseGranite=0 ;

/* // Contrôle de la neige */
/* #declare snow_density=1.0; */
/* #declare snow_color=<0.95, 0.95, 1.0>; */
/* // Contrôle du roc */
/* #declare rock_normal=1.0; // Amplitude des bandes et de la granulation */
/* #declare rock_color=<0.7,0.6,0.3>;  */
/* #declare rock_scale=1.0; // Échelle globale */
/* #declare rock_pigment_scale=1.0; // Utilisez pour mettre à l'échelle les bandes seulement, sans le grain */
/* // Adhérence de la neige */
/* #declare adhesion=0.6; // Le défaut est 0.5 */


#include "slope_patterns.inc"
#include "geomorph_txtr.inc"

#declare maincamera =
camera
{
	location	<0,  0, -0.1>	// <X Y Z>
	angle 	gl_angle
	up		y              	// which way is +up <X Y Z>
	right		aspect_ratio*x         	// which way is +right <X Y Z> and aspect ratio
	look_at	<0, 0, 0> 	// point center of view at this point <X Y Z>
}

// Lights positions updated 2004-09-14 for trying to get more 
// consistency with the GL preview

#declare light=<1,0.97,0.94>; // Couleur de la lumière (<rouge, vert, bleu>)
#declare sun= sphere {<0,1,0>,100
         pigment{rgb 3*light} // Habituellement blanc, mais peut être coloré même avec un tel niveau de luminosité lorsque certaines composantes sont nulles ou presque
         finish {ambient 1} // La couleur de la sphère sera produite d'elle-même plutôt que par réflexion des autres sources lumineuses
         }
#declare Sun =
light_source
{
  0*x // Position de la source lumineuse (déplacée plus bas)
  color rgb light  // Couleur
  looks_like {sun} // Donne au Soleil la forme de "sun" (sphère)
}

#declare sky_gradient =
        pigment {
            gradient y
            color_map {
                [0 color rgb sky_density*<0.8,0.8,1>] // Ligne d'horizon brumeuse
                [0.15 color sky_density*(<0.1,0.3,1>)] // Début du bleu pur
                [1.0 color sky_density*(<0.1,0.2,1>)] // Bleu pur
            }
        }

#declare clouds =
pigment {
  granite
    turbulence 0.2
    color_map {
    [0.4 rgbt <0,0,0,1>] // Le "t" dans rgbt réfère à "transmettre" - cette couleur est totalement transparente et sans aucune teinte
      //              [0.7 rgbt <0,0,0,1>] // Remplacez la ligne précédente par celle-ci pour des nuages plus petits
      [0.95 rgb <0.7,0.7,0.8>] // Un blanc cassé légèrement bleuté
      [1.0 rgb <0.7,0.7,0.8>]
      }
}

sky_sphere {pigment {sky_gradient scale <1.0, 1.0, 1.0> }
  pigment {clouds scale <1.0,0.4,1.0> rotate <0.0,-10,0.0>}}


//    Eau sombre
#declare water = plane {y, 0
    texture {
        pigment {color rgbt<0.0,water_density*0.15,water_density*0.35,0.2>}
       normal {bumps 0.1 translate clock scale 3*<2.5,1,1> rotate <0,-30,0>}
       finish {specular 0.9 roughness 0.05
             reflection .5 ambient 0.1 diffuse 0.3 }
    }
}

#declare Haze =
fog
{
  fog_type   2   // 1 est un brouillard uniforme, 2 est un brouillard de sol (ground fog)
  distance fog_distance
  color      rgbt <0.6,0.6,0.8,fog_transparency > // Voile bleuté
  fog_offset fog_altitude
  fog_alt fog_altitude
  turbulence 0.8
}

fog {Haze}

plane {y,0 texture {shore_granite}}

light_source {Sun  translate <2000,2000,-1000> rotate <rot_x, rot_y, rot_z> }

#declare hf=
height_field {
    png "carte-pov.png"
    smooth
    scale < 1.0, 0.3, 1.0 >
    translate <-.5, 0.0, -.5>
}

object {hf texture {forest_rock_snow_bushes}}


camera {maincamera
		translate <0+tr_x,0.09+tr_y,tr_z>
		rotate <rot_x,0+rot_y,0+rot_z>
    }
