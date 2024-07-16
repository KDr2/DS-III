#include <SFML/Graphics.hpp>
#include <cstdlib>
#include <vector>

void gen_pi_points(std::vector<sf::Vector2f> &points, int width, int height) {
    points.clear();
    int count = 1000;
    if (width > count) {
        count = width;
    }
    int step = count > width ? count / width : 1;
    int in_cnt = 0;
    for (int i = 0 ; i < count; i++) {
        auto x = (rand() % 100 - 50) / 50.0f;
        auto y = (rand() % 100 - 50) / 50.0f;
        bool in = x * x + y * y < 1;
        if (in) {
                in_cnt ++;
        }
        if (i % step == 0) {
            float pi = 4.0f * in_cnt / i;
            points.push_back(sf::Vector2f(i / step, pi / 3.1415926 * height / 2.0f));
        }
    }
}

int main() {
    int w_w = 200, w_h = 200;
    sf::RenderWindow window(sf::VideoMode(200, 200), "SFML works!");
    window.setFramerateLimit(30);

    std::vector<sf::Vector2f> points;

    while (window.isOpen())
    {
        sf::Event event;
        while (window.pollEvent(event))
        {
            if (event.type == sf::Event::Closed)
                window.close();
            if (event.type == sf::Event::Resized) {
                // update the view to the new size of the window
                w_w = event.size.width;
                w_h = event.size.height;
                sf::FloatRect visibleArea(0, 0, event.size.width, event.size.height);
                window.setView(sf::View(visibleArea));
                gen_pi_points(points, w_w, w_h);
            }
            if (event.type == sf::Event::KeyPressed)
                gen_pi_points(points, w_w, w_h);
        }

        window.clear();
        sf::RectangleShape shape(sf::Vector2f(w_w, 1.f));
        shape.setFillColor(sf::Color::Green);
        shape.setPosition(sf::Vector2f(0.f, w_h / 2.0f));
        window.draw(shape);

        for(auto p: points) {
            sf::CircleShape point(1);
            point.setPosition(p);
            point.setFillColor(sf::Color::Red);
            window.draw(point);
        }

        window.display();
    }

    return 0;
}
