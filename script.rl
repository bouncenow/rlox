class Person {
    init(name) {
        this.name = name;
        this.age = 33;
    }

    definition() {
        print this.name + ", age: " + this.age;
    }
}

var person = Person("John");
person.definition();
print person.init("test");