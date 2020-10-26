use std::cmp;

/// Trapped rain water
pub fn rained(it: &[usize]) -> usize {
    let mut right = Vec::with_capacity(it.len());
    let mut left = Vec::with_capacity(it.len());
    for (l, r) in it.iter().copied().zip(it.iter().copied().rev()) {
        let last_left = left.last().copied().unwrap_or(0);
        left.push(cmp::max(last_left, l));
        let last_right = right.last().copied().unwrap_or(0);
        right.push(cmp::max(last_right, r));
    }

    left.into_iter()
        .zip(right.into_iter().rev())
        .zip(it.iter().copied())
        .map(|((l, r), i)| cmp::min(l, r) - i)
        .sum()
}

#[derive(Copy, Clone)]
enum At {
    L(usize),
    R(usize),
}
use At::*;

struct Node<'a> {
    at: At,
    cur: &'a [usize],
    liters: f64,
}

/// Rain for `hours`
///
/// Complexity for serial version is O(n log n), O(n^2) worst case, O(n) best case,
/// where n is the number of segments
///
///
/// Divide and conquer solved with a fork-join task (left or right) // TODO
///
/// Merge sort parallel algorithm variant
///
/// TODO: Describe algorithm, compute parallel complexity
///
/// Linear maps describing are:
/// (\[N\], +Q) -> +Q, where N natural, +Q positive rational and 0
///
/// Every linear map describe the liters for each full or totally empty "waterscape"
///
///
pub fn rain(hours: usize, it: &[usize]) -> Vec<f64> {
    let hours = hours as f64;
    match it.len() {
        0 => return vec![],
        1 => return vec![hours],
        _ => (),
    }

    let mut stack = vec![Node {
        at: R(0),
        cur: it,
        liters: hours * it.len() as f64,
    }];

    // TODO: implement join by sorted chunks
    let mut result = vec![0.0; it.len()];

    // TODO: fork-join
    // Diffusion equation is implemented by divide and conquer over merge sort structure
    while let Some(n) = stack.pop() {
        node(hours, n, &mut result, &mut stack);
    }

    result
}

fn node<'a>(
    hours: f64,
    Node {
        at,
        cur,
        mut liters,
    }: Node<'a>,
    result: &mut [f64],
    stack: &mut Vec<Node<'a>>,
) {
    // Max height segment
    let (max_n, max_v) = cur
        .iter()
        .enumerate()
        .max_by_key(|(_, x)| *x)
        .map(|(i, x)| (i + 1, *x))
        .expect("minimum one");

    // Result iterator
    let r_it = match at {
        R(n) => &mut result[n..n + cur.len()],
        L(n) => &mut result[n - cur.len()..n],
    };

    // Available space
    let t_it = cur.iter().map(|x| (max_v - x) as f64);

    // Total available space
    let total: f64 = t_it.clone().sum();

    if (total - liters).abs() < f64::EPSILON {
        // Litters is equal, the topology is the same
        for (r, t) in r_it.iter_mut().zip(t_it) {
            *r = t;
        }
    } else if total < liters {
        // Litters is greater, the topology is the same
        let diff = liters - total;

        // Calculate portion for each
        let portion = diff / cur.len() as f64;
        for (r, t) in r_it.iter_mut().zip(t_it) {
            *r = t + portion;
        }
    } else {
        // Litters is less, unknown topology

        // Middle left and right
        let mut l_num = max_n;
        let r_num = l_num;

        // Middle litters
        let mut middle = 0.0;

        // Find middle and litters in middle
        let mut is_max = true;
        let mut last_max = 0;
        for (i, (avail, r)) in t_it
            .clone()
            .take(l_num)
            .zip(r_it.iter_mut())
            .rev()
            .enumerate()
        {
            let diff = hours - avail;
            if -f64::EPSILON < diff {
                middle += diff;
            } else if (diff).abs() > f64::EPSILON {
                if !is_max {
                    for i in r_it
                        .iter_mut()
                        .take(max_n)
                        .rev()
                        .skip(last_max + 1)
                        .take(i - last_max)
                    {
                        liters += *i;
                        *i = 0.0;
                    }
                    middle = r_it
                        .iter()
                        .rev()
                        .take(last_max + 1)
                        .map(|x| hours - x)
                        .sum();
                    l_num = max_n - (last_max + 1);
                }

                break;
            }

            is_max = avail < f64::EPSILON;
            if is_max {
                last_max = i;
            } else {
                liters -= avail;
                *r = avail;
            }
            l_num -= 1;
        }

        // In middle water
        let m = middle / 2.0;

        // Left water
        let mut l_liters = l_num as f64 * hours + m;
        let l_total: f64 = t_it.clone().take(l_num).sum();
        // Right water
        let mut r_liters = (cur.len() - r_num) as f64 * hours + m;
        let r_total: f64 = t_it.clone().skip(r_num).sum();

        // Propagate surplus over start
        let diff = liters - (l_liters + r_liters);
        let (l_at, r_at) = match at {
            R(n) => {
                l_liters += diff;
                (n + l_num, n + r_num)
            }
            L(n) => {
                r_liters += diff;
                (n - cur.len() + l_num, n - cur.len() + r_num)
            }
        };

        // Rectify surplus to left
        if l_liters > l_total {
            let diff = l_liters - l_total;
            r_liters += diff;
            l_liters -= diff;
        }

        // Rectify surplus to right
        if r_liters > r_total {
            let diff = r_liters - r_total;
            r_liters -= diff;
            l_liters += diff;
        }

        // New node left
        let left = &cur[..l_num];
        if !left.is_empty() {
            if left.len() == 1 {
                r_it[l_num - 1] = l_liters;
            } else {
                stack.push(Node {
                    at: L(l_at),
                    cur: left,
                    liters: l_liters,
                });
            }
        }

        // New node right
        let right = &cur[r_num..];
        if !right.is_empty() {
            if right.len() == 1 {
                r_it[r_num] = r_liters;
            } else {
                stack.push(Node {
                    at: R(r_at),
                    cur: right,
                    liters: r_liters,
                });
            }
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn identity() {
        let res = vec![1, 1];

        assert_eq!(rain(1, &res), vec![1.0, 1.0]);
        assert_eq!(rain(2, &res), vec![2.0, 2.0]);

        let res = vec![1, 1, 1];
        assert_eq!(rain(1, &res), vec![1.0, 1.0, 1.0]);

        let res = vec![1];
        assert_eq!(rain(2, &res), vec![2.0]);

        for size in 0..200 {
            let res = vec![1; size];
            assert_eq!(rain(1, &res), vec![1.0; size]);
        }
    }

    #[test]
    fn greater() {
        let res = vec![1, 9, 1];

        for size in 8..200 {
            let part = (3 * size - 16) as f64 / 3f64;
            assert_eq!(rain(size, &res), vec![8.0 + part, part, 8.0 + part]);
        }

        let res = vec![1, 9, 1, 9, 1];

        for size in 8..200 {
            let part = (5 * size - 24) as f64 / 5f64;
            assert_eq!(
                rain(size, &res),
                vec![8.0 + part, part, 8.0 + part, part, 8.0 + part]
            );
        }

        let res = vec![1, 9, 1, 6, 1];

        for size in 8..200 {
            let part = (5 * size - 27) as f64 / 5f64;
            assert_eq!(
                rain(size, &res),
                vec![8.0 + part, part, 8.0 + part, 3.0 + part, 8.0 + part]
            );
        }

        let res = vec![1, 9, 9, 6, 1];

        for size in 8..200 {
            let part = (5 * size - 19) as f64 / 5f64;
            assert_eq!(
                rain(size, &res),
                vec![8.0 + part, part, part, 3.0 + part, 8.0 + part]
            );
        }

        let res = vec![1, 9, 9, 6];

        for size in 8..200 {
            let part = (4 * size - 11) as f64 / 4f64;
            assert_eq!(rain(size, &res), vec![8.0 + part, part, part, 3.0 + part]);
        }
    }

    #[test]
    fn less() {
        let res = vec![1, 9, 1];
        assert_eq!(rain(1, &res), vec![1.5, 0.0, 1.5]);
        assert_eq!(rain(2, &res), vec![3.0, 0.0, 3.0]);
        assert_eq!(rain(5, &res), vec![7.5, 0.0, 7.5]);

        let res = vec![1, 9, 1, 1];
        assert_eq!(rain(1, &res), vec![1.5, 0.0, 1.25, 1.25]);
        assert_eq!(rain(2, &res), vec![3.0, 0.0, 2.5, 2.5]);
    }

    #[test]
    fn another() {
        let res = vec![1, 9, 1, 9];
        assert_eq!(rain(1, &res), vec![1.5, 0.0, 2.5, 0.0]);

        let res = vec![9, 9, 1, 9];
        assert_eq!(rain(1, &res), vec![0.0, 0.0, 4.0, 0.0]);

        let res = vec![8, 9, 1, 9];
        assert_eq!(rain(1, &res), vec![1.0, 0.0, 3.0, 0.0]);

        let res = vec![8, 9, 1, 9, 9];
        assert_eq!(rain(1, &res), vec![1.0, 0.0, 4.0, 0.0, 0.0]);

        let res = vec![8, 5, 1, 12, 9, 2, 1, 12, 1];
        let part = 1.0 / 6.0;
        let exp = vec![part, 3.0 + part, 7.0 + part, 0.0, 0.0, 5.5, 6.5, 0.0, 4.5];
        assert_eq!(exp.iter().sum::<f64>(), 3.0 * res.len() as f64);
        assert_eq!(rain(3, &res), exp);
    }

    #[test]
    fn failed() {
        let res = vec![1, 8, 8, 1];
        assert_eq!(rain(1, &res), vec![2.0, 0.0, 0.0, 2.0]);

        let res = vec![8, 1, 8, 8, 1];
        assert_eq!(rain(1, &res), vec![0.0, 3.0, 0.0, 0.0, 2.0]);

        let res = vec![5, 8, 7, 7, 8, 1];
        assert_eq!(rain(1, &res), vec![2.0, 0.0, 1.0, 1.0, 0.0, 2.0]);

        let res = vec![5, 9, 7, 7, 8, 1];
        assert_eq!(rain(1, &res), vec![1.5, 0.0, 1.0, 1.0, 0.0, 2.5]);

        let res = vec![5, 8, 7, 7, 9, 1];
        assert_eq!(rain(1, &res), vec![2.5, 0.0, 1.0, 1.0, 0.0, 1.5]);
    }

    #[test]
    fn test() {
        let res = vec![5, 8, 7, 7, 8, 1];

        assert_eq!(rained(&res), 2);
    }

    #[test]
    fn possible() {
        let res = vec![
            1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20,
        ];
        let part = 5.0 / 6.0;

        #[rustfmt::skip]
        let exp = vec![
            5.0 + part, 4.0 + part, 3.0 + part, 2.0 + part, 1.0 + part, part,
            0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0
        ];
        assert_eq!(5 + 4 + 3 + 2 + 1 + 5 /* 5.0 / 6.0 * 6.0 */, res.len());
        let r = rain(1, &res);
        assert_eq!(r, exp);
    }

    #[test]
    fn double_possible() {
        #[rustfmt::skip]
        let res = vec![
            1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20,
            1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20,
        ];
        let part = 3.0 / 4.0;
        let part2 = 11.0 / 12.0;

        #[rustfmt::skip]
            let exp = vec![
            5.0 + part, 4.0 + part, 3.0 + part, 2.0 + part, 1.0 + part, part,
            0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,

            5.0 + part2, 4.0 + part2, 3.0 + part2, 2.0 + part2, 1.0 + part2, part2,
            0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0
        ];
        assert_eq!(30.0 + (3.0 / 4.0 + 11.0 / 12.0) * 6.0, res.len() as f64);
        let r = rain(1, &res);
        assert_eq!(r, exp);
    }

    #[test]
    fn left_propagate() {
        let res = vec![5, 8, 8, 6, 6, 8, 8, 1];
        let exp = vec![2.0, 0.0, 0.0, 2.0, 2.0, 0.0, 0.0, 2.0];
        assert_eq!(exp.iter().sum::<f64>(), res.len() as f64);
        assert_eq!(rain(1, &res), exp);

        let res = vec![5, 8, 8, 8, 6, 6, 8, 1];
        let exp = vec![2.5, 0.0, 0.0, 0.0, 2.0, 2.0, 0.0, 1.5];
        assert_eq!(exp.iter().sum::<f64>(), res.len() as f64);
        assert_eq!(rain(1, &res), exp);

        let res = vec![5, 8, 6, 6, 8, 8, 8, 1];
        let exp = vec![1.5, 0.0, 2.0, 2.0, 0.0, 0.0, 0.0, 2.5];
        assert_eq!(exp.iter().sum::<f64>(), res.len() as f64);
        assert_eq!(rain(1, &res), exp);
    }

    #[test]
    fn double_possible_left() {
        #[rustfmt::skip]
            let res = vec![
            6, 5 ,4, 3, 2, 1,
            1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20,
            1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20,
        ];
        let part = 11.0 / 20.0;
        let part2 = 11.0 / 12.0;

        #[rustfmt::skip]
            let exp = vec![
            0.0,
            part, 1.0 + part, 2.0 + part, 3.0 + part, 4.0 + part,
            4.0 + part, 3.0 + part, 2.0 + part, 1.0 + part, part,
            0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
            0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
            0.0,
            5.0 + part2, 4.0 + part2, 3.0 + part2, 2.0 + part2, 1.0 + part2, part2,
            0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0
        ];
        assert_eq!(
            (4.0 + 3.0 + 2.0 + 1.0) * 3.0 + 5.0 + part * 10.0 + part2 * 6.0,
            res.len() as f64
        );
        let r = rain(1, &res);
        assert_eq!(r, exp);
    }

    #[test]
    fn double_possible_left_2hours() {
        let res = vec![1, 2, 3, 4, 5, 6];

        let part = 2.0 / 5.0;

        let exp = vec![4.0 + part, 3.0 + part, 2.0 + part, 1.0 + part, part, 0.0];
        assert_eq!(part * 5.0 + 10.0, (res.len() * 2) as f64);
        let r = rain(2, &res);
        assert_eq!(r, exp);

        #[rustfmt::skip]
        let res = vec![
            6, 5 ,4, 3, 2, 1,
            1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20,
            1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20,
        ];
        let part = (51.0 - (6.0 + 5.0 + 4.0 + 3.0 + 2.0 + 1.0) * 2.0) / 13.0;
        let part2 = (41.0 - (8.0 + 7.0 + 6.0 + 5.0 + 4.0 + 3.0 + 2.0 + 1.0)) / 9.0;

        #[rustfmt::skip]
        let exp = vec![
            1.0 + part,
            2.0 + part, 3.0 + part, 4.0 + part, 5.0 + part, 6.0 + part,
            6.0 + part, 5.0 + part, 4.0 + part, 3.0 + part, 2.0 + part,
            1.0 + part, part,
            0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
            8.0 + part2, 7.0 + part2, 6.0 + part2, 5.0 + part2, 4.0 + part2, 3.0 + part2,
            2.0 + part2, 1.0 + part2, part2,
            0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0
        ];
        assert_eq!(
            (6.0 + 5.0 + 4.0 + 3.0 + 2.0 + 1.0) * 3.0 + 8.0 + 7.0 + part * 13.0 + part2 * 9.0,
            res.len() as f64 * 2.0
        );
        let r = rain(2, &res);
        assert_eq!(r, exp);
    }

    #[test]
    fn another_left() {
        let res = vec![1, 2, 3, 4, 5, 6, 7, 7, 7];

        let part = 1.0 / 2.0;

        #[rustfmt::skip]
            let exp = vec![
            5.0 + part, 4.0 + part, 3.0 + part, 2.0 + part, 1.0 + part, part,
            0.0, 0.0, 0.0
        ];
        assert_eq!(part * 6.0 + 15.0, (res.len() * 2) as f64);
        let r = rain(2, &res);
        assert_eq!(r, exp);

        let res = vec![1, 2, 3, 4, 5, 6, 7, 7, 7, 5];

        let part = 1.0 / 2.0;

        #[rustfmt::skip]
            let exp = vec![
            5.0 + part, 4.0 + part, 3.0 + part, 2.0 + part, 1.0 + part, part,
            0.0, 0.0, 0.0,
            2.0
        ];
        assert_eq!(part * 6.0 + 15.0 + 2.0, (res.len() * 2) as f64);
        let r = rain(2, &res);
        assert_eq!(r, exp);

        let res = vec![1, 2, 1, 7, 0, 6, 7, 5];

        let part = 2.0 / 3.0;
        let part2 = 1.0 / 2.0;

        #[rustfmt::skip]
            let exp = vec![
            2.0 + part, 1.0 + part, 2.0 + part,
            0.0,
            6.0 + part2, part2,
            0.0,
            2.0
        ];
        assert_eq!(
            part * 3.0 + part2 * 2.0 + 5.0 + 6.0 + 2.0,
            (res.len() * 2) as f64
        );

        const ERROR: f64 = 1e-15;
        let r = rain(2, &res);
        assert!(r
            .into_iter()
            .zip(exp.into_iter())
            .all(|(r, exp)| (r - exp).abs() < ERROR));
    }

    #[test]
    fn left_right() {
        let res = vec![1, 2, 3, 4, 5, 6, 7, 7, 7, 1];

        #[rustfmt::skip]
            let exp = vec![
            5.0, 4.0, 3.0, 2.0, 1.0, 0.0,
            0.0, 0.0, 0.0,
            5.0
        ];
        assert_eq!(exp.iter().sum::<f64>(), (res.len() * 2) as f64);
        let r = rain(2, &res);
        assert_eq!(r, exp);

        let res = vec![1, 2, 3, 4, 5, 6, 7, 6, 6, 7, 1];

        #[rustfmt::skip]
            let exp = vec![
            5.0, 4.0, 3.0, 2.0, 1.0, 0.0,
            0.0, 1.0, 1.0, 0.0,
            5.0
        ];
        assert_eq!(exp.iter().sum::<f64>(), (res.len() * 2) as f64);
        let r = rain(2, &res);
        assert_eq!(r, exp);
    }
}
