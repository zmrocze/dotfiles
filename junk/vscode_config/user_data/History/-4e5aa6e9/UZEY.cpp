#include "string"

using namespace std;

class Interface
{

public:
    virtual bool open(string name) = 0;
    virtual bool close() = 0;

protected:
    std::fstream file;
    
}



int main() {
}