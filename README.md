# Quasi-Biennial-Oscillation

Analyzing the Quasi-Biennial-Oscillation time series. The description of the time series is in the
Freie Universität Berlin at
[http://www.geo.fu-berlin.de/en/met/ag/strat/produkte/qbo/](http://www.geo.fu-berlin.de/en/met/ag/strat/produkte/qbo/)

This program parses only the Singapore data (the **singapore.dat** file) because the samples taken
at the other equatorial stations, at the Canton Island and at Gan in the Maldives Islands, which are
present also at the above page, have been closed for more than 40 years.

Furthermore, the **singapore.dat** input file is chosen because it is more detailed, it has measures
of the speed at the

      10, 12, 15, 20, 25, 30, 35, 40, 45, 50, 60, 70, 80, 90, 100 hPascal

instead of only measures of the speed for

      10, 15, 20, 30, 40, 50, 70 hPascal

as in the other input files.

# WIP

This project is a *work in progress*. The implementation is *incomplete* and subject to change. The documentation can be inaccurate.

# How to Run:

Run:

      sbt run

To install `sbt`, see [http://www.scala-sbt.org/download.html](http://www.scala-sbt.org/download.html)

# Example report:

In the first version of this program, it has two reports, one of this has this heading:

      Reporting the Quasi-Bienal-Oscillation according to date first.

and this is a sample of it, where the first column is the date of the sample, and the next columns are the list of pairs of **the atmospheric pressure** and its corresponding **wind speed in 0.1 meters/second units** (a negative wind speed means that the wind is easterly, and a positive speed means the wind is westerly).

      Reporting the Quasi-Bienal-Oscillation according to date first.
      198701  10: -164  12: -300  15: -374  20: -326  25: -289  30: -260  35: -192  40:  -86  45:  -18  50:   39  60:  102  70:   57  80:   11  90:  -34 100:    0
      198702  10: -167  12: -336  15: -356  20: -307  25: -280  30: -266  35: -209  40: -113  45:  -21  50:   32  60:   87  70:   61  80:   46  90:  -29 100:    0
      198703  10: -144  12: -325  15: -331  20: -311  25: -309  30: -271  35: -219  40: -171  45:  -72  50:   19  60:   73  70:   22  80:  -37  90:    3 100:    0
      198704  10:  -35  12: -125  15: -350  20: -328  25: -303  30: -270  35: -244  40: -236  45: -188  50:  -74  60:   51  70:   -4  80:  -63  90:  -26 100:    0
      198705  10:  116  12:   74  15:  -45  20: -295  25: -289  30: -268  35: -283  40: -266  45: -248  50: -192  60:  -12  70:   -2  80:  -15  90:  -29 100:    0
      198706  10:   82  12:   69  15:   60  20:  -11  25: -152  30: -247  35: -367  40: -273  45: -293  50: -231  60: -154  70:  -74  80:  -52  90:  -63 100:    0
      198707  10:   45  12:   79  15:  108  20:   93  25:   21  30:  -63  35: -179  40: -281  45: -311  50: -267  60: -204  70: -145  80: -108  90:  -61 100:    0
      198708  10:  116  12:  153  15:  145  20:  124  25:  112  30:   92  35:   43  40:  -14  45:  -85  50: -224  60: -213  70: -148  80: -100  90: -135 100:    0
      198709  10:  140  12:  162  15:  145  20:  145  25:  165  30:  138  35:  131  40:   91  45:   48  50:  -62  60: -209  70: -133  80:  -90  90: -114 100:    0
      198710  10:  144  12:  155  15:  139  20:  133  25:  128  30:  117  35:  124  40:  113  45:  114  50:   68  60:  -18  70: -115  80: -110  90: -139 100:    0
      198711  10:  116  12:  130  15:  147  20:  124  25:   93  30:   77  35:   86  40:   93  45:   82  50:   83  60:   49  70:  -37  80:  -97  90:  -87 100:    0
      198712  10:  145  12:  141  15:  106  20:   86  25:   58  30:   57  35:   70  40:   78  45:   76  50:   66  60:   62  70:   -2  80:  -27  90:  -52 100:    0
      198801  10:   66  12:   83  15:  101  20:   89  25:   60  30:   54  35:   46  40:   91  45:  111  50:   91  60:   74  70:    7  80:  -65  90:  -63 100:    0
      198802  10:  -43  12:   -8  15:    5  20:   12  25:   30  30:   46  35:   93  40:   95  45:  112  50:   77  60:   43  70:   49  80:   -4  90:  -19 100:    0
      198803  10:  -89  12:  -94  15: -158  20:  -53  25:   39  30:   78  35:  110  40:  100  45:   88  50:   74  60:   20  70:   20  80:   19  90:  -65 100:    0
      198804  10: -147  12: -186  15: -215  20: -150  25:   14  30:   70  35:   96  40:   94  45:   94  50:   68  60:   49  70:   19  80:  -38  90:  -70 100:    0
      198805  10: -210  12: -250  15: -281  20: -258  25:  -63  30:   50  35:  109  40:  123  45:  115  50:   92  60:   58  70:   22  80:  -33  90: -100 100:    0
      198806  10: -243  12: -275  15: -274  20: -297  25: -204  30:  -23  35:   75  40:  105  45:  112  50:   98  60:   97  70:   61  80:   11  90:  -37 100:    0
      198807  10: -245  12: -295  15: -309  20: -322  25: -251  30: -102  35:   32  40:   96  45:  104  50:  105  60:  100  70:   60  80:   34  90:   -4 100:    0
      198808  10: -312  12: -329  15: -372  20: -327  25: -224  30:  -63  35:   60  40:   99  45:  125  50:  103  60:   66  70:   57  80:   50  90:  -20 100:    0
      198809  10: -346  12: -387  15: -373  20: -334  25: -217  30:  -50  35:   62  40:   94  45:  130  50:  104  60:   84  70:   53  80:    7  90:  -19 100:    0
      198810  10: -341  12: -366  15: -371  20: -321  25: -231  30:  -81  35:   30  40:   93  45:  116  50:  100  60:   76  70:   54  80:   26  90:  -24 100:    0
      198811  10: -180  12: -325  15: -334  20: -306  25: -232  30: -101  35:    4  40:  110  45:  109  50:  106  60:   82  70:   58  80:   25  90:  -51 100:    0
      198812  10: -122  12: -277  15: -335  20: -306  25: -248  30:  -99  35:   11  40:  136  45:  149  50:  120  60:   63  70:   23  80:  -19  90:   -7 100:    0
      198901  10: -244  12: -338  15: -353  20: -328  25: -261  30:  -84  35:   29  40:  118  45:  138  50:  117  60:  120  70:   68  80:   13  90:  -59 100:    0
      198902  10: -348  12: -340  15: -339  20: -310  25: -205  30:  -40  35:   60  40:  109  45:  126  50:  102  60:   84  70:   37  80:   -3  90:  -85 100:    0
      198903  10: -366  12: -374  15: -359  20: -303  25: -184  30:  -38  35:   59  40:  124  45:  136  50:  117  60:  101  70:   41  80:   15  90:   -2 100:    0
      198904  10: -378  12: -394  15: -353  20: -323  25: -286  30: -150  35:   10  40:   97  45:   98  50:  100  60:   77  70:   67  80:   33  90:    2 100:    0
      198905  10: -329  12: -403  15: -347  20: -327  25: -309  30: -250  35: -154  40:  -20  45:   72  50:   92  60:  119  70:   77  80:   11  90:  -42 100:    0
      198906  10: -128  12: -305  15: -367  20: -338  25: -325  30: -281  35: -235  40: -172  45:  -41  50:   -3  60:   62  70:   58  80:   18  90:   16 100:    0
      198907  10:  -17  12:  -87  15: -346  20: -363  25: -364  30: -306  35: -268  40: -231  45: -132  50:  -36  60:   66  70:   42  80:   23  90:  -33 100:    0
      198908  10:   34  12:   20  15:  -91  20: -350  25: -324  30: -300  35: -281  40: -238  45: -145  50:  -58  60:   31  70:   36  80:   31  90:   59 100:    0
      198909  10:   35  12:   19  15:  -76  20: -346  25: -316  30: -310  35: -298  40: -239  45: -197  50: -119  60:  -13  70:   41  80:   73  90:   13 100:    0
      198910  10:   84  12:   53  15:   12  20: -235  25: -311  30: -307  35: -291  40: -250  45: -224  50: -152  60:  -37  70:    3  80:   30  90:   11 100:    0
      198911  10:  157  12:  121  15:   73  20:  -95  25: -297  30: -306  35: -264  40: -239  45: -219  50: -166  60:  -88  70:  -20  80:  -19  90:  -42 100:    0
      198912  10:  212  12:  249  15:  218  20:  127  25: -147  30: -282  35: -285  40: -248  45: -244  50: -184  60:  -83  70:    3  80:    7  90:  -50 100:    0
      199001  10:  149  12:  169  15:  173  20:  131  25:   64  30:  -42  35: -247  40: -292  45: -279  50: -214  60: -102  70:  -14  80:  -15  90:  -44 100:    0
      199002  10:  157  12:  180  15:  192  20:  146  25:   79  30:  -32  35: -149  40: -265  45: -273  50: -210  60: -118  70:  -81  80:  -71  90: -100 100:    0
      199003  10:  182  12:  222  15:  212  20:  181  25:  177  30:  109  35:   31  40:  -81  45: -242  50: -221  60:  -73  70:  -31  80:  -34  90: -116 100:    0
      199004  10:  154  12:  159  15:  181  20:  166  25:  152  30:  124  35:   98  40:   49  45:  -49  50: -142  60: -136  70:  -81  80: -102  90:  -43 100:    0
      199005  10:   64  12:  114  15:  120  20:  136  25:  160  30:  124  35:   98  40:   85  45:   87  50:   22  60: -105  70: -109  80:  -87  90:  -92 100:    0
      199006  10:   48  12:   93  15:  130  20:  172  25:  185  30:  152  35:  154  40:  122  45:  145  50:  118  60:   43  70:  -50  80: -101  90:  -90 100:    0
      ......

# Example visualizations:

The program also generates the visualization of the oscillation in the wind speed according to each atmospheric pressure. Below are these visualizations for some -not all- of the atmospheric pressures.

![Equatorial wind speed oscillation at atmospheric pressure 10 hPascal](example_visualizations/quasiBienalOscillation_10.png)
![Equatorial wind speed oscillation at atmospheric pressure 12 hPascal](example_visualizations/quasiBienalOscillation_12.png)
![Equatorial wind speed oscillation at atmospheric pressure 15 hPascal](example_visualizations/quasiBienalOscillation_15.png)
![Equatorial wind speed oscillation at atmospheric pressure 20 hPascal](example_visualizations/quasiBienalOscillation_20.png)
![Equatorial wind speed oscillation at atmospheric pressure 25 hPascal](example_visualizations/quasiBienalOscillation_25.png)
![Equatorial wind speed oscillation at atmospheric pressure 45 hPascal](example_visualizations/quasiBienalOscillation_45.png)
![Equatorial wind speed oscillation at atmospheric pressure 60 hPascal](example_visualizations/quasiBienalOscillation_60.png)
![Equatorial wind speed oscillation at atmospheric pressure 70 hPascal](example_visualizations/quasiBienalOscillation_70.png)
![Equatorial wind speed oscillation at atmospheric pressure 80 hPascal](example_visualizations/quasiBienalOscillation_80.png)
![Equatorial wind speed oscillation at atmospheric pressure 90 hPascal](example_visualizations/quasiBienalOscillation_90.png)

