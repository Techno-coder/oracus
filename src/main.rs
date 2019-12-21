fn main() {
	let text = r#"
#include <iostream>
#include <algorithm>
#include <vector>
#include <cassert>
#include <stack>
#include <climits>
#include <utility>
#include <map>

using namespace std;

int main() {
    cin.tie(nullptr);
    ios::sync_with_stdio(false);

	int subtask, n_sides;
    cin >> subtask >> n_sides;

    vector<char> side_colours(n_sides);
    for (int i = 0; i < n_sides; ++i)
        cin >> side_colours[i];

    // <start, end>, colour
    vector<pair<pair<int, int>, int>> diagonals(n_sides - 3);
    for (int i = 0; i < n_sides - 3; ++i) {
        int start, end, colour;
        cin >> start >> end >> colour;
        --start; --end;

        assert(start != end);
        if (start > end) swap(start, end);
        diagonals[i] = {{start, end}, colour};
    }

    {
        // Verify no diagonals are overlapping
        // End events must be processed before start events
        // We also process the start event of longer diagonals first
        // and process the end event last with equivalent vertices
        // <vertex>, <is_start, <length, diagonal>>
        vector<pair<int, pair<bool, pair<int, int>>>> events((n_sides - 3) * 2);
        for (int i = 0; i < diagonals.size(); ++i) {
            int length = diagonals[i].first.second - diagonals[i].first.first;
            events[i * 2] = {diagonals[i].first.first, {true, {INT_MAX - length, i}}};
            events[i * 2 + 1] = {diagonals[i].first.second, {false, {length, i}}};
        }
        sort(events.begin(), events.end());

        stack<int> s;
        for (const auto& event : events) {
            if (event.second.first) {
                // Is startpoint
                s.push(event.second.second.second);
            } else {
                // Is endpoint
                if (s.top() != event.second.second.second) {
                    cout << "neispravna triangulacija" << endl;
                    return 0;
                } else s.pop();
            }
        }
        assert(s.empty());
    }

    // neighbours[node] = neighbour -> colour
    vector<map<int, char>> neighbours(n_sides);
    for (int start = 0; start < n_sides - 1; ++start) {
        neighbours[start][start + 1] = side_colours[start] - '0';
        neighbours[start + 1][start] = side_colours[start] - '0';
    }
    neighbours[n_sides - 1][0] = side_colours[n_sides - 1] - '0';
    neighbours[0][n_sides - 1] = side_colours[n_sides - 1] - '0';

    // Add diagonals
    for (const auto& diagonal : diagonals) {
        int start = diagonal.first.first;
        int end = diagonal.first.second;
        neighbours[start][end] = diagonal.second;
        neighbours[end][start] = diagonal.second;
    }

    for (int node = 0; node < n_sides; ++node) {
        vector<int> node_neighbours;
        for (const auto& edge : neighbours[node]) {
            int neighbour = edge.first;
            if (neighbour < node)
                // Order the neighbour vertices by offsetting
                // any neighbours lower than current node
                neighbour += n_sides;
            node_neighbours.push_back(neighbour);
        }
        sort(node_neighbours.begin(), node_neighbours.end());

        for (int i = 0; i < node_neighbours.size() - 1; ++i) {
            int neighbour = node_neighbours[i];
            int other = node_neighbours[i + 1];

            // Reverse the ordering transformation
            if (neighbour >= n_sides) neighbour -= n_sides;
            if (other >= n_sides) other -= n_sides;

            // Every two adjacent neighbours MUST be connected by a diagonal.
            // Suppose that they were not, then the two diagonals connected to the node
            // form a triangular which no other lines can pass through (as triangles
            // cannot overlap). But, since the two neighbours are not connected together,
            // this forms a rectangular region which breaks the triangulation rule.
            // Hence, this contradicts our assumption. QED.
            if (neighbours[node][neighbour] == neighbours[neighbour][other] ||
                    neighbours[neighbour][other] == neighbours[other][node] ||
                    neighbours[other][node] == neighbours[node][neighbour]) {
                cout << "neispravno bojenje" << endl;
                return 0;
            }
        }
    }

    cout << "tocno" << endl;
}
	"#;

	match oracus::parser::parse(&text) {
		Ok(roots) => println!("{:?}", roots),
		Err(error) => {
			println!("{:?}", error.node);
			println!("{}", &text[error.span.0..]);
		}
	}
}
