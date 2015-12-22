#include <boost/python.hpp>

char const* greet()
{
   return "hello, world";
}

struct World
{
    World(): msg("") {}
    World(std::string msg): msg(msg) {}
    void set(std::string msg) { this->msg = msg; }
    std::string greet() { return msg; }
    std::string msg;
};

BOOST_PYTHON_MODULE(bst_py0)
{
    using namespace boost::python;
    def("greet", greet);

    class_<World>("World", init<std::string>())
        .def("greet", &World::greet)
        .def("set", &World::set);
}
