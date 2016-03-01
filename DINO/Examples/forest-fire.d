// Forest fire simulation program (see http://rosettacode.org/wiki/Forest_fire)
// Usage: dino forest-fire.d [<number iterations>]
// Requires a terminal supporting ANSI escape codes.

val RED = "\033[1;31m", YELLOW = "\033[1;33m", GREEN = "\033[1;32m";
val RESET = "\033[0m", HOME = "\033[H", CLEAR = "\033[2J";
val Empty = 0, Tree = 1, Heating = 2, Burning = 3;
val pix = ["  ", GREEN @ "木", YELLOW @ "木", RED @ "木"];

expose math.rand;

class forest (height, width) {
  val p = 0.01, f = 0.001;
  var spot = [height: []];;

  for (var i = 0; i < height; i++) {
    spot[i] = [width: 0];
    for (var j = 0; j < width; j++)
      spot[i][j] = rand () <= 0.55 ? Tree : Empty;
  }
  
  fun step {
    var heat = [];
    for (var i = 0; i < height; i++)
      for (var j = 0; j < width; j++)
	pmatch (spot[i][j]) {
	case Empty + 0: if (rand () < p) spot[i][j] = Tree;
	case Tree + 0: if (rand () < f) spot[i][j] = Heating;
	case Heating + 0: spot[i][j] = Burning; ins (heat, [i,j]);
	case _: spot[i][j] = Empty;
	}
    for (var k = 0; k < #heat; k++) {
      val [i, j] = heat[k];
      for (var i1 = i - 1; i1 <= i + 1; i1++)
	if (0 <= i1 && i1 < height)
	  for (var j1 = j - 1; j1 <= j + 1; j1++)
	    if (0 <= j1 && j1 < width && spot[i1][j1] == Tree)
	      spot[i1][j1] = Heating;
    }	      
  }
  
  fun show {
    for (var i = 0; i < height; i++)
      for (var j = 0; j < width; j++)
        put (pix[spot[i][j]], j == width - 1 ? "\n" : "");
  }
}

var height, width;

fun set_screen_size {
  var f = popen ("stty size", "r");
  val rows = fscan (f), cols = fscan (f);
  close (f);
  if (rows < 3 && cols < 3) {
    fputln (stderr, "too small screen - ", rows, " x ", cols); exit (0);
  }
  height = rows - 2; width = cols / 2 - 1;
}

val iter = #argv == 0 ? 200 : int (argv[0]);

try {
  set_screen_size ();
  val f = forest (height, width);
  put (CLEAR);
  for (var i = 0; i < iter; i++) {
    putln (HOME, i); f.show (); f.step ();
  }
} catch (sys.sigint) {
}

putln (HOME, CLEAR, RESET); exit (0);
