#include "stdio.h"
#include "string"

using namespace std;

class Person
{
    string name;
    string surname;

    // without this declaration name and surname fields are not in scope in the below definition
    friend std::ostream& operator<<(std::ostream&, const Person &);

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

ostream & operator<<(ostream & out, const Person & p) {
   out << "(" << p.name << "," << p.surname << ")";
   return out;
}


int main() {
    int n;
    scanf("%d\n", &n);

    

}