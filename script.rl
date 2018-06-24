for (var i = 0; i < 3; i = i + 1) {
    fun inside() {
        print i;
        for (var j = -3; j < i; j = j + 1) {
            print j;
            if (j == -2) {
                break;
            }
        }
    }
    if (i == 1) {
        break;
    }
    inside();
}