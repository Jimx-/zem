#ifndef _ROPE_H_
#define _ROPE_H_

#include <array>
#include <memory>
#include <optional>
#include <string>
#include <vector>

namespace zem {

class Rope {
    friend class RopeBuilder;

private:
    static constexpr size_t MIN_LEAF = 511;
    static constexpr size_t MAX_LEAF = 1024;
    static constexpr size_t MIN_CHILDREN = 4;
    static constexpr size_t MAX_CHILDREN = 8;

    struct BaseNode {
        unsigned int height;
        size_t len;

        explicit BaseNode(unsigned int height, size_t len)
            : height(height), len(len)
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
        std::vector<PNode> children;

        explicit InternalNode(std::vector<PNode>&& children)
            : BaseNode(children[0]->height + 1, 0), children(children)
        {
            len = 0;
            for (auto&& p : children)
                len += p->len;
        }

        virtual bool is_ok_child() const
        {
            return children.size() >= MIN_CHILDREN;
        }

        virtual const std::vector<PNode>* get_children() const
        {
            return &children;
        }

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
        std::string val;

        explicit LeafNode(std::string_view val)
            : BaseNode(0, val.length()), val(val)
        {}

        virtual bool is_ok_child() const { return len >= MIN_LEAF; }

        virtual const std::vector<PNode>* get_children() const
        {
            return nullptr;
        }

        virtual std::string_view get_value() const { return val; }

        virtual std::optional<std::string> push_str(std::string_view str);

        virtual PNode clone() const { return std::make_shared<LeafNode>(val); }
    };

    PNode root;

    Rope(PNode&& root) : root(root) {}

    static PNode merge_nodes(const std::vector<PNode>& lhs,
                             const std::vector<PNode>& rhs);

    static PNode merge_leaves(const PNode& lhs, const PNode& rhs);

    static PNode concat(const PNode& lhs, const PNode& rhs);

    static size_t find_leaf_split(std::string_view str, size_t minsplit);

public:
    class Cursor {
        friend Rope;

    private:
        static constexpr size_t CURSOR_CACHE_SIZE = 4;

        PNode root;
        size_t position;
        std::array<std::pair<BaseNode*, int>, CURSOR_CACHE_SIZE> cache;

        std::optional<std::string> leaf;
        size_t leaf_offset;

        void descend();

        std::optional<std::string> next_leaf();

    public:
        explicit Cursor(PNode root, size_t position)
            : root(root), position(position), leaf(std::nullopt), leaf_offset(0)
        {
            descend();
        }

        size_t get_position() const { return position; }
        std::optional<std::string> get_leaf_value() const { return leaf; }
        size_t get_leaf_offset() const { return position - leaf_offset; }
    };

    static constexpr size_t npos = (size_t)-1;

    size_t length() const { return root->len; }

    void clear();

    void edit(size_t start, size_t end, std::string_view new_str);

    std::string substr(size_t pos, size_t len = npos) const;
};

class RopeBuilder {
    friend class Rope;

private:
    std::vector<std::vector<Rope::PNode>> stack;

    void push(Rope::PNode node);
    void push_slice(Rope::PNode node, size_t start, size_t end);

    Rope::PNode pop();

public:
    void push_leaf(std::string_view str);

    Rope build();
};

} // namespace zem

#endif
