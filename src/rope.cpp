#include "rope.h"

#include <algorithm>

namespace zem {

namespace detail {

struct LinesMetric : Metric {
    virtual size_t measure(BaseNode* node, size_t len) const
    {
        return node->lines;
    }

    virtual bool is_boundary(std::string_view str, off_t offset) const
    {
        if (offset == 0) return false;
        return str[offset - 1] == '\n';
    }

    virtual std::optional<off_t> next(std::string_view str, off_t offset) const
    {
        auto pos = str.find('\n', offset);
        if (pos == str.npos) return {};
        return pos + 1;
    }
};

LeafNode::LeafNode(std::string_view val)
    : BaseNode(0, val.length(), 0), val(val)
{
    lines = std::count(val.begin(), val.end(), '\n');
}

std::optional<std::string> LeafNode::push_str(std::string_view str)
{
    val += str;
    if (val.length() <= MAX_LEAF) {
        lines = std::count(val.begin(), val.end(), '\n');
        len = val.length();
        return {};
    }

    auto split = find_leaf_split(val, MIN_LEAF);
    auto right = val.substr(split);
    val = val.substr(0, split);
    len = val.length();
    lines = std::count(val.begin(), val.end(), '\n');
    return right;
}

size_t LeafNode::find_leaf_split(std::string_view str, size_t minsplit)
{
    auto splitpoint = std::min(MAX_LEAF, str.length() - MIN_LEAF);
    auto pos = str.substr(minsplit - 1, splitpoint - minsplit).rfind('\n');

    if (pos == str.npos) return splitpoint;
    return minsplit + pos;
}

} // namespace detail

detail::PNode Rope::merge_nodes(const std::vector<detail::PNode>& lhs,
                                const std::vector<detail::PNode>& rhs)
{
    auto nchild = lhs.size() + rhs.size();
    std::vector<detail::PNode> children;

    children.reserve(nchild);
    children.insert(children.end(), lhs.begin(), lhs.end());
    children.insert(children.end(), rhs.begin(), rhs.end());

    if (nchild <= detail::InternalNode::MAX_CHILDREN) {
        return std::make_shared<detail::InternalNode>(std::move(children));
    } else {
        auto split = std::min(detail::InternalNode::MAX_CHILDREN,
                              nchild - detail::InternalNode::MIN_CHILDREN);
        std::vector<detail::PNode> left{children.begin(),
                                        children.begin() + split};
        std::vector<detail::PNode> right{children.begin() + split,
                                         children.end()};
        return std::make_shared<detail::InternalNode>(
            std::vector<detail::PNode>{
                std::make_shared<detail::InternalNode>(std::move(left)),
                std::make_shared<detail::InternalNode>(std::move(right))});
    }
}

detail::PNode Rope::merge_leaves(const detail::PNode& lhs,
                                 const detail::PNode& rhs)
{
    if (lhs->is_ok_child() && rhs->is_ok_child())
        return std::make_shared<detail::InternalNode>(
            std::vector<detail::PNode>{lhs, rhs});

    auto new_str = lhs->push_str(rhs->get_value());

    if (new_str)
        return std::make_shared<detail::InternalNode>(
            std::vector<detail::PNode>{
                lhs, std::make_shared<detail::LeafNode>(*new_str)});

    return lhs;
}

detail::PNode Rope::concat(const detail::PNode& lhs, const detail::PNode& rhs)
{
    auto h1 = lhs->height;
    auto h2 = rhs->height;

    if (h1 < h2) {
        auto* rchild = rhs->get_children();

        if (h1 == h2 - 1 && lhs->is_ok_child())
            return merge_nodes({lhs}, *rchild);

        auto new_rope = concat(lhs, (*rchild)[0]->clone());
        std::vector<detail::PNode> rest{rchild->begin() + 1, rchild->end()};
        if (new_rope->height == h2 - 1)
            return merge_nodes({new_rope}, rest);
        else
            return merge_nodes(*new_rope->get_children(), rest);
    } else if (lhs->height == rhs->height) {
        if (lhs->is_ok_child() && rhs->is_ok_child())
            return std::make_shared<detail::InternalNode>(
                std::vector<detail::PNode>{lhs, rhs});

        if (!h1) return merge_leaves(lhs, rhs);

        return merge_nodes(*lhs->get_children(), *rhs->get_children());
    } else {
        auto* lchild = lhs->get_children();

        if (h2 == h1 - 1 && rhs->is_ok_child())
            return merge_nodes(*lchild, {rhs});

        auto new_rope = concat((*lchild)[lchild->size() - 1]->clone(), rhs);
        std::vector<detail::PNode> rest{lchild->begin(), lchild->end() - 1};
        if (new_rope->height == h1 - 1)
            return merge_nodes(rest, {new_rope});
        else
            return merge_nodes(rest, *new_rope->get_children());
    }
}

void Rope::edit(size_t start, size_t end, std::string_view new_str)
{
    RopeBuilder b;

    auto slice_start = 0;
    auto slice_end = std::min(start, root->len);
    b.push_slice(root, slice_start, slice_end);

    if (!new_str.empty()) b.push_leaf(new_str);

    slice_start = std::min(root->len, end);
    slice_end = root->len;
    b.push_slice(root, slice_start, slice_end);

    *this = b.build();
}

std::string Rope::substr(size_t pos, size_t len) const
{
    size_t end;
    std::string str;

    if (len == npos)
        end = root->len;
    else
        end = pos + len;

    str.reserve(end - pos);

    Cursor cursor(root, pos);

    while (cursor.get_position() < end) {
        auto leaf = cursor.get_leaf_value();
        auto start_pos = cursor.get_leaf_offset();
        auto len =
            std::min(end - cursor.get_position(), leaf->length() - start_pos);

        cursor.next_leaf();

        str += leaf->substr(start_pos, len);
    }

    return str;
}

size_t Rope::count_node_lines(detail::PNode node, size_t start, size_t end)
{
    if (start >= end) return 0;

    if (start == 0 && end == node->len) {
        return node->lines;
    }

    if (node->height == 0) {
        return std::count(node->get_value().begin() + start,
                          node->get_value().begin() + end, '\n');
    } else {
        size_t offset = 0;
        size_t lines = 0;

        for (auto&& p : *node->get_children()) {
            if (end <= offset) break;

            auto child_start = offset;
            auto child_end = offset + p->len;

            auto rec_start = std::max(child_start, start);
            auto rec_end = std::min(child_end, end);

            lines += count_node_lines(p, rec_start - offset, rec_end - offset);
            offset += p->len;
        }

        return lines;
    }
}

off_t Rope::next_lines(off_t pos, size_t count)
{
    Cursor cursor(root, pos);

    while (count > 0 && cursor.next_line())
        count--;

    return cursor.get_position();
}

size_t Rope::count_lines(size_t pos, size_t len) const
{
    size_t end;
    std::string str;

    if (len == npos)
        end = root->len;
    else
        end = pos + len;

    return count_node_lines(root, pos, end);
}

void Rope::clear()
{
    RopeBuilder b;
    *this = b.build();
}

void RopeBuilder::push(detail::PNode node)
{
    for (;;) {
        if (stack.empty()) {
            stack.emplace_back(std::vector<detail::PNode>{std::move(node)});
            return;
        }

        auto last_height = stack.back().front()->height;

        if (last_height < node->height) {
            node = Rope::concat(pop(), node);
        } else if (last_height == node->height) {
            auto& tos = stack.back();

            if (tos.back()->is_ok_child() && node->is_ok_child())
                tos.emplace_back(std::move(node));
            else if (node->height == 0) {
                auto new_leaf = tos.back()->push_str(node->get_value());
                if (new_leaf)
                    tos.push_back(
                        std::make_shared<detail::LeafNode>(*new_leaf));
            } else {
                auto last = std::move(tos.back());
                tos.pop_back();

                auto* lhs = last->get_children();
                auto* rhs = node->get_children();

                auto nchild = lhs->size() + rhs->size();
                std::vector<detail::PNode> children;

                children.reserve(nchild);
                children.insert(children.end(), lhs->begin(), lhs->end());
                children.insert(children.end(), rhs->begin(), rhs->end());

                if (nchild <= detail::InternalNode::MAX_CHILDREN) {
                    tos.emplace_back(std::make_shared<detail::InternalNode>(
                        std::move(children)));
                } else {
                    auto split =
                        std::min(detail::InternalNode::MAX_CHILDREN,
                                 nchild - detail::InternalNode::MIN_CHILDREN);
                    std::vector<detail::PNode> left{children.begin(),
                                                    children.begin() + split};
                    std::vector<detail::PNode> right{children.begin() + split,
                                                     children.end()};
                    tos.emplace_back(std::make_shared<detail::InternalNode>(
                        std::move(left)));
                    tos.emplace_back(std::make_shared<detail::InternalNode>(
                        std::move(right)));
                }
            }

            if (tos.size() < detail::InternalNode::MAX_CHILDREN) return;

            node = pop();
        } else {
            stack.emplace_back(std::vector<detail::PNode>{std::move(node)});
            return;
        }
    }
}

void RopeBuilder::push_slice(detail::PNode node, size_t start, size_t end)
{
    if (start >= end) return;

    if (start == 0 && end == node->len) {
        push(node->clone());
        return;
    }

    if (node->height == 0) {
        push_leaf(node->get_value().substr(start, end - start));
    } else {
        size_t offset = 0;

        for (auto&& p : *node->get_children()) {
            if (end <= offset) break;

            auto child_start = offset;
            auto child_end = offset + p->len;

            auto rec_start = std::max(child_start, start);
            auto rec_end = std::min(child_end, end);

            push_slice(p, rec_start - offset, rec_end - offset);
            offset += p->len;
        }
    }
}

void RopeBuilder::push_leaf(std::string_view str)
{
    push(std::make_shared<detail::LeafNode>(str));
}

detail::PNode RopeBuilder::pop()
{
    auto nodes = std::move(stack.back());
    stack.pop_back();

    if (nodes.size() == 1) return std::move(nodes.front());

    return std::make_shared<detail::InternalNode>(std::move(nodes));
}

Rope RopeBuilder::build()
{
    if (stack.empty()) return Rope{std::make_shared<detail::LeafNode>("")};

    auto node = pop();
    while (!stack.empty())
        node = Rope::concat(pop(), node);

    return Rope{std::move(node)};
}

void Rope::Cursor::descend()
{
    auto node = root;
    auto offset = 0;

    while (node->height > 0) {
        auto* children = node->get_children();

        int i;
        for (i = 0; i < children->size(); i++) {
            auto nextoff = offset + (*children)[i]->len;
            if (nextoff > position) break;
            offset = nextoff;
        }

        auto idx = node->height - 1;
        if (idx < CURSOR_CACHE_SIZE) cache[idx] = std::make_pair(node.get(), i);

        node = (*children)[i];
    }

    leaf = node->get_value();
    leaf_offset = offset;
}

void Rope::Cursor::descend_metric(size_t measure, const detail::Metric* metric)
{
    auto node = root;
    auto offset = 0;

    while (node->height > 0) {
        auto* children = node->get_children();

        int i;
        for (i = 0; i < children->size(); i++) {
            auto child = (*children)[i];
            auto child_m = metric->measure(child.get(), child->len);
            if (child_m >= measure) break;
            offset += child->len;
            measure -= child_m;
        }

        auto idx = node->height - 1;
        if (idx < CURSOR_CACHE_SIZE) cache[idx] = std::make_pair(node.get(), i);

        node = (*children)[i];
    }

    position = offset;
    leaf = node->get_value();
    leaf_offset = offset;
}

std::optional<std::string> Rope::Cursor::next_leaf()
{
    if (!leaf) return {};

    position = leaf_offset + leaf->length();

    for (int i = 0; i < cache.size(); i++) {
        if (!cache[i].first) {
            leaf = {};
            return leaf;
        }

        auto* node = cache[i].first;
        auto j = cache[i].second;

        if (j + 1 < node->get_children()->size()) {
            cache[i].second = j + 1;
            auto downlink = (*node->get_children())[j + 1];

            for (int k = i - 1; k >= 0; k--) {
                cache[k].first = downlink.get();
                cache[k].second = 0;
                downlink = (*downlink->get_children())[0];
            }

            leaf = downlink->get_value();
            leaf_offset = position;
            return leaf;
        }
    }

    if (leaf_offset + leaf->length() == root->len) {
        leaf = {};
        return leaf;
    }

    descend();
    return leaf;
}

std::optional<off_t> Rope::Cursor::next_in_leaf(const detail::Metric* metric)
{
    auto off_in_leaf = position - leaf_offset;
    auto leaf_next = metric->next(*leaf, off_in_leaf);
    if (!leaf_next) return {};
    if (*leaf_next == leaf->length() && leaf_offset + *leaf_next == root->len)
        next_leaf();
    else
        position = leaf_offset + *leaf_next;
    return position;
}

std::optional<off_t> Rope::Cursor::next(const detail::Metric* metric)
{
    if (position >= root->len || !leaf) {
        leaf = {};
        return {};
    }

    auto offset = next_in_leaf(metric);
    if (offset) return offset;

    if (!next_leaf()) return {};
    offset = next_in_leaf(metric);
    if (offset) return offset;

    auto measure = measure_leaf(position, metric);
    descend_metric(measure + 1, metric);

    offset = next_in_leaf(metric);
    if (offset) return offset;

    position = root->len;
    leaf = {};
    return {};
}

std::optional<off_t> Rope::Cursor::next_line()
{
    detail::LinesMetric metric;
    return next(&metric);
}

size_t Rope::Cursor::measure_leaf(size_t pos, const detail::Metric* metric)
{
    auto node = root;
    size_t measure = 0;

    while (node->height) {
        for (auto&& p : *node->get_children()) {
            auto len = p->len;

            if (pos < len) break;

            pos -= len;
            measure += metric->measure(p.get(), len);
        }
    }

    return measure;
}

} // namespace zem
