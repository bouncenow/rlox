for (var i = 0; i < 100; i = i + 1) {
    for (var j = i; j > -4; j = j - 1) {
        print "Inside loop";
        print (j * j);
        if (j == 0) {
            break;
        }
    }
    if (i > 3) {
        break;
    }
    print i;
}