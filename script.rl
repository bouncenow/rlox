class DevonshireCream {
  serveOn() {
    return "Scones";
  }
}

var m = DevonshireCream();
m.cream_name = "abcde";

print m.cream_name;

m.cream_name = "abv";
print m.cream_name;

fun creamFactory(creamName) {
    var instance = DevonshireCream();
    instance.cream_name = creamName;
    return instance;
}

var instance = creamFactory("gfh");
print instance.cream_name;