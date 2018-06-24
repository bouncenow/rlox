fun compose(f, g) {
    return fun (e) {
        return f(g(e));
    };
}

var result = compose(fun (a) {
    return a + 1;
}, fun (b) {
    return b * 2;
});

print result(3);

(fun (b) { var c = b * 2; print c; })(10);