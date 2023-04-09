#include "string"
#include <fstream>

using namespace std;

class Interface
{

public:
    virtual bool open(string name) = 0;
    virtual bool close() = 0;

protected:
    std::fstream file;

};

class FileInterface 
{

public:
    bool open(string name) {
        
    }

};

int main() {
}