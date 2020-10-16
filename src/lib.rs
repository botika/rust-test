use std::f64;
use std::usize;

/// Rain for `hours`
///
/// Complexity is O(n log n) where n is the number of segments
///
/// The complexity is much higher than the typical trapping rain water
/// which can be solved with 2 pointers.
///
/// I was inspired by a bubble sort, but recursion in this case is necessary
/// or at least I don't know how to stack it because it need a local max height position
///
/// Local maximums are sought and the water is distributed in each direction,
/// stabilizing it in each case.
///
/// Every step is documented
pub fn rain(hours: usize, it: &[usize]) -> Vec<f64> {
    _rain(hours, (hours * it.len()) as f64, it, false)
}

fn _rain(hours: usize, liters: f64, it: &[usize], left: bool) -> Vec<f64> {
    let size = it.len();

    // End
    if size == 1 {
        return vec![liters];
    } else if size == 0 {
        return vec![];
    }

    // Max height segment
    let max = it
        .iter()
        .enumerate()
        .max_by_key(|(_, x)| *x)
        .map(|(i, x)| (i, *x))
        .expect("minimum one");

    // Available space
    let total_vec: Vec<usize> = it.iter().map(|x| (max.1 - x)).collect();

    // Total available space
    let total: f64 = total_vec.iter().map(|x| *x as f64).sum();

    if (total - liters).abs() < f64::EPSILON {
        // Litters as equal, the topology is the same
        total_vec.into_iter().map(|x| x as f64).collect()
    } else if total < liters {
        // Litters as greater, the topology is the same
        let mut res = Vec::with_capacity(total_vec.len());
        let diff = liters - total;

        // Calculate portion for each
        let portion = diff / size as f64;

        // Sum them
        for total in total_vec {
            res.push(total as f64 + portion);
        }
        res
    } else {
        // Litters as less, unknown topology
        // Distributed the litters

        // In half water
        let m = hours as f64 / 2.0;

        // Left water
        let l_num = max.0;
        let mut l_liters = l_num as f64 * hours as f64 + m;
        let l_total: f64 = total_vec[..l_num].iter().map(|x| *x as f64).sum();

        // Right water
        let r_num = total_vec.len() - (max.0 + 1);
        let mut r_liters = r_num as f64 * hours as f64 + m;
        let r_total: f64 = total_vec[l_num + 1..].iter().map(|x| *x as f64).sum();

        // Difference between real
        let diff = liters - (l_liters + r_liters);
        if left {
            l_liters += diff;
        } else {
            r_liters += diff;
        }

        // Move surplus
        if l_liters > l_total {
            let d = l_liters - l_total;
            l_liters -= d;
            r_liters += d;
        } else if r_liters > r_total {
            let d = r_liters - r_total;
            r_liters -= d;
            l_liters += d;
        }

        let mut result = Vec::with_capacity(it.len());

        // Distribute left litters
        result.extend(_rain(hours, l_liters, &it[..l_num], false));

        // Recursion is necessary since you have to mark the position of the local maximum.
        // Local max height uncovered by water
        result.push(0.0);

        // Distribute right litters
        result.extend(_rain(hours, r_liters, &it[l_num + 1..], true));

        result
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
}
