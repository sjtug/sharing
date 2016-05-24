use std::mem;

type Link<T> = Option<Box<List<T>>>;

struct List<T> {
    elem: T,
    next: Link<T>,
}

pub struct Stack<T> {
    list: Link<T>,
}

impl<T> Stack<T> {
    fn new() -> Stack<T> {
        Stack { list: None }
    }

    fn push(&mut self, elem: T) {
        let list = mem::replace(&mut self.list, None);
        self.list = Some(Box::new(List { elem: elem, next: list }));
    }

    fn pop(&mut self) -> Option<T> {
        let list = mem::replace(&mut self.list, None);
        match list {
            None => None,
            Some(boxed) => {
                let node = *boxed;
                self.list = node.next;
                Some(node.elem)
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::Stack;

    #[test]
    fn it_works() {
    }

    #[test]
    fn test_new() {
        let stack: Stack<()> = Stack::new();
    }

    #[test]
    fn test_push() {
        let mut stack = Stack::new();
        stack.push(1);
        stack.push(2);
        stack.push(3);
    }

    #[test]
    fn test_pop() {
        let mut stack = Stack::new();
        stack.push(1);
        stack.push(2);
        stack.push(3);
        assert_eq!(stack.pop(), Some(3));
        assert_eq!(stack.pop(), Some(2));
        assert_eq!(stack.pop(), Some(1));
        assert_eq!(stack.pop(), None);
    }
}
