#include "stdio.h"
#include "string"

using namespace std;

class Person
{
    string name;
    string surname;

public:
    Person(string name1, string surname1) {
        name = name1;
        surname = surname1;
    }

public:
    string getName() {
        return name;
    }

};

int main() {
    int n;
    scanf("%d\n", &n);

    

}