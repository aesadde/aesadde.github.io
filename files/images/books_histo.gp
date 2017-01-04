reset
unset log
unset label
set autoscale

set terminal png enhanced font 'Helvetica, 9'

set output 'downloads_histo.png'

set boxwidth 0.95 absolute
set style fill solid noborder
set style histogram gap 1
set style data histogram

set xlabel 'Year'
set ylabel 'Books'

set xtics border in scale 0,0 nomirror rotate by -90 autojustify
set xtics  norangelimit
set xtics   ()

plot "books.dat" using 2:xticlabels(1) notitle linecolor rgb "blue"
