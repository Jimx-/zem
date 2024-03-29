#ifndef _ROPE_H_
#define _ROPE_H_

#include <array>
#include <memory>
#include <optional>
#include <string>
#include <vector>

namespace zem {

namespace detail {

struct BaseNode {
    unsigned int height;
    size_t len;
    size_t lines;

    explicit BaseNode(unsigned int height, size_t len, size_t lines)
        : height(height), len(len), lines(lines)
    {}

    virtual bool is_ok_child() const = 0;
    virtual const std::vector<std::shared_ptr<BaseNode>>*
    get_children() const = 0;
    virtual std::string_view get_value() const { return ""; }

    virtual std::optional<std::string> push_str(std::string_view str) = 0;

    virtual std::shared_ptr<BaseNode> clone() const = 0;
};

using PNode = std::shared_ptr<BaseNode>;

struct InternalNode : BaseNode {
    static constexpr size_t MIN_CHILDREN = 4;
    static constexpr size_t MAX_CHILDREN = 8;

    std::vector<PNode> children;

    explicit InternalNode(std::vector<PNode>&& children)
        : BaseNode(children[0]->height + 1, 0, 0), children(children)
    {
        for (auto&& p : children) {
            len += p->len;
            lines += p->lines;
        }
    }

    virtual bool is_ok_child() const { return children.size() >= MIN_CHILDREN; }

    virtual const std::vector<PNode>* get_children() const { return &children; }

    virtual std::optional<std::string> push_str(std::string_view str)
    {
        return {};
    }

    virtual PNode clone() const
    {
        return std::make_shared<InternalNode>(std::vector<PNode>{children});
    }
};

struct LeafNode : BaseNode {
    static constexpr size_t MIN_LEAF = 511;
    static constexpr size_t MAX_LEAF = 1024;

    std::string val;

    explicit LeafNode(std::string_view val);

    static size_t find_leaf_split(std::string_view str, size_t minsplit);

    virtual bool is_ok_child() const { return len >= MIN_LEAF; }

    virtual const std::vector<PNode>* get_children() const { return nullptr; }
    virtual std::string_view get_value() const { return val; }

    virtual std::optional<std::string> push_str(std::string_view str);

    virtual PNode clone() const { return std::make_shared<LeafNode>(val); }
};

struct Metric {
    virtual size_t measure(detail::BaseNode* node, size_t len) const = 0;

    virtual bool is_boundary(std::string_view str, off_t offset) const = 0;

    virtual std::optional<off_t> next(std::string_view str,
                                      off_t offset) const = 0;
};

} // namespace detail

class Rope {
    friend class RopeBuilder;

private:
    detail::PNode root;

    Rope(detail::PNode&& root) : root(root) {}

    static detail::PNode merge_nodes(const std::vector<detail::PNode>& lhs,
                                     const std::vector<detail::PNode>& rhs);

    static detail::PNode merge_leaves(const detail::PNode& lhs,
                                      const detail::PNode& rhs);

    static detail::PNode concat(const detail::PNode& lhs,
                                const detail::PNode& rhs);

    static size_t count_node_lines(detail::PNode node, size_t start,
                                   size_t end);

public:
    class Cursor {
        friend Rope;

    private:
        static constexpr size_t CURSOR_CACHE_SIZE = 4;

        detail::PNode root;
        size_t position;
        std::array<std::pair<detail::BaseNode*, int>, CURSOR_CACHE_SIZE> cache;

        std::optional<std::string> leaf;
        size_t leaf_offset;

        void descend();
        void descend_metric(size_t measure, const detail::Metric* metric);

        std::optional<std::string> next_leaf();

        std::optional<off_t> next_in_leaf(const detail::Metric* metric);

        std::optional<off_t> next(const detail::Metric* metric);

        size_t measure_leaf(size_t pos, const detail::Metric* metric);

    public:
        explicit Cursor(detail::PNode root, size_t position)
            : root(root), position(position), leaf(std::nullopt), leaf_offset(0)
        {
            descend();
        }

        size_t get_position() const { return position; }
        std::optional<std::string> get_leaf_value() const { return leaf; }
        size_t get_leaf_offset() const { return position - leaf_offset; }

        std::optional<off_t> next_line();
    };

    static constexpr size_t npos = (size_t)-1;

    size_t length() const { return root->len; }

    void clear();

    void edit(size_t start, size_t end, std::string_view new_str);

    std::string substr(size_t pos, size_t len = npos) const;

    off_t next_lines(off_t pos, size_t count = 1);

    size_t count_lines(size_t pos, size_t len = npos) const;
};

class RopeBuilder {
    friend class Rope;

private:
    std::vector<std::vector<detail::PNode>> stack;

    void push(detail::PNode node);
    void push_slice(detail::PNode node, size_t start, size_t end);

    detail::PNode pop();

public:
    void push_leaf(std::string_view str);

    Rope build();
};

} // namespace zem

#endif
