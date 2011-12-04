# PARAM
ALPHA=1.7
OCTAVE=6
SEED=42

.PHONY : carte-pov.png

carte-pov.png:
	../genperlin -seed ${SEED} -alpha ${ALPHA} -octave ${OCTAVE} -grey -save | convert - carte-pov.png

map.png: carte-pov.png map.pov
	povray +Omap.png -D +Q9 +W800 +H600 map.pov
