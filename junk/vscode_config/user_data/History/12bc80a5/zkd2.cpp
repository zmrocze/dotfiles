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

ostream & operator<<(ostream & out, const Point & point) {
   out << "(" << point.x << "," << point.y << ")";  // access private data
   return out;
}
 

};

int main() {
    int n;
    scanf("%d\n", &n);

    

}