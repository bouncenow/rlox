class Eclair < Box {
  cook() {
    super.cook();
    print "Pipe full of crème pâtissière.";
  }
}

var eclair = Eclair();
eclair.cook();