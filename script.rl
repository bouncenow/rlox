class Person {
    init(name) {
        this.name = name;
        return;
        this.age = 33;
    }

    definition() {
        print this.name + ", age: " + this.age;
    }
}

var person = Person("John");
person.age = 35;
person.definition();
print person.init("test");