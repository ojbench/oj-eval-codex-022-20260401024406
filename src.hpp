// Submission header for interactive problem 1936 (Guess)
#pragma once
#include <bits/stdc++.h>
using namespace std;

extern int query(int, int, int);

static const int MOD = 998244353;

namespace guess_impl {
struct TripleKey {
    int a, b, c;
};

// Compute values for 5 indices using 10 triple queries
// Returns mapping from index -> value; indices provided in ids (size 5)
static bool solve_base5(const array<int,5>& ids, unordered_map<int,long long>& out_val) {
    // Map triple (by sorted indices) -> observed sum
    map<tuple<int,int,int>, long long> obs;
    vector<pair<TripleKey,long long>> triples;
    for (int i = 0; i < 5; ++i) for (int j = i+1; j < 5; ++j) for (int k = j+1; k < 5; ++k) {
        int x = ids[i], y = ids[j], z = ids[k];
        long long s = query(x, y, z);
        obs[make_tuple(x,y,z)] = s;
        triples.push_back({{x,y,z}, s});
    }

    // Frequency of sums
    map<long long,int> freq;
    for (auto &p : triples) freq[p.second]++;

    // Collect by multiplicity
    vector<long long> cnt1, cnt2, cnt3;
    for (auto &kv : freq) {
        if (kv.second == 1) cnt1.push_back(kv.first);
        else if (kv.second == 2) cnt2.push_back(kv.first);
        else if (kv.second == 3) cnt3.push_back(kv.first);
    }
    if (cnt1.size() != 3 || cnt2.size() != 2 || cnt3.size() != 1) {
        return false; // degenerate case (unlikely)
    }
    sort(cnt1.begin(), cnt1.end()); // s13 < s24 < s35
    sort(cnt2.begin(), cnt2.end()); // s14 < s25
    // Identify sums
    long long s13 = cnt1[0];
    long long s24 = cnt1[1];
    long long s35 = cnt1[2];
    long long s14 = cnt2[0];
    long long s25 = cnt2[1];
    long long s15 = cnt3[0];

    // Solve values
    long long v3_times2 = s13 + s35 - s15;
    if ((v3_times2 & 1LL) != 0) return false;
    long long v3 = v3_times2 / 2;
    long long v1 = s13 - v3;
    long long v5 = s35 - v3;
    long long v4 = s14 - v1;
    long long v2 = s24 - v4;

    array<long long,5> vals = {v1,v2,v3,v4,v5};

    // Try all assignments of these values to the 5 indices to match observed triples
    array<int,5> idx = ids;
    array<int,5> perm = {0,1,2,3,4};
    // Precompute all triples among 5 by positions
    vector<tuple<int,int,int>> posTrip;
    for (int i = 0; i < 5; ++i) for (int j = i+1; j < 5; ++j) for (int k = j+1; k < 5; ++k) posTrip.emplace_back(i,j,k);

    // Store observed value for each triple by concrete indices for quick check
    // Already in obs map

    sort(perm.begin(), perm.end());
    do {
        bool ok = true;
        for (auto &t : posTrip) {
            int pi, pj, pk; tie(pi,pj,pk) = t;
            int xi = idx[pi], xj = idx[pj], xk = idx[pk];
            long long si = vals[perm[pi]];
            long long sj = vals[perm[pj]];
            long long sk = vals[perm[pk]];
            long long mn = min(si, min(sj, sk));
            long long mx = max(si, max(sj, sk));
            long long expected = mn + mx;
            auto it = obs.find(make_tuple(xi,xj,xk));
            if (it == obs.end() || it->second != expected) { ok = false; break; }
        }
        if (ok) {
            for (int t = 0; t < 5; ++t) out_val[idx[t]] = vals[perm[t]];
            return true;
        }
    } while (next_permutation(perm.begin(), perm.end()));

    return false;
}
}

int guess(int n, int Taskid) {
    (void)Taskid;
    vector<long long> A(n+1, LLONG_MIN);

    // Choose first 5 indices
    array<int,5> ids = {1,2,3,4,5};
    unordered_map<int,long long> baseVals;
    bool ok = guess_impl::solve_base5(ids, baseVals);
    if (!ok) {
        // Fallback: try a shifted set (very unlikely to be needed)
        if (n >= 6) {
            array<int,5> ids2 = {2,3,4,5,6};
            ok = guess_impl::solve_base5(ids2, baseVals);
        }
        if (!ok) {
            // Last resort: assign zeros to avoid crash (should not happen)
            long long res = 0, pow = 233 % MOD;
            for (int i = 1; i <= n; ++i) {
                res = (res + 0LL * pow) % MOD;
                pow = (pow * 233) % MOD;
            }
            return (int)res;
        }
    }

    // Fill known base values
    for (auto &kv : baseVals) A[kv.first] = kv.second;

    // Select three references U<V<W among known base values
    vector<pair<long long,int>> refs;
    refs.reserve(baseVals.size());
    for (auto &kv : baseVals) refs.push_back({kv.second, kv.first});
    sort(refs.begin(), refs.end());
    int idxU = refs[0].second;
    int idxV = refs[2].second; // median among 5
    int idxW = refs.back().second;
    long long U = A[idxU], V = A[idxV], W = A[idxW];
    long long sumUV = U + V;
    long long sumUW = U + W;

    // Compute remaining elements
    for (int i = 1; i <= n; ++i) if (A[i] == LLONG_MIN) {
        long long sUW = query(i, idxU, idxW);
        if (sUW > sumUW) {
            // X > W => s = U + X
            A[i] = sUW - U;
        } else if (sUW < sumUW) {
            // X < U => s = W + X
            A[i] = sUW - W;
        } else {
            // U < X < W, need s with U,V
            long long sUV = query(i, idxU, idxV);
            if (sUV > sumUV) {
                // X > V => s = U + X
                A[i] = sUV - U;
            } else if (sUV < sumUV) {
                // X < U (should not happen if sUW == sumUW), fallback compute with V,W
                long long sVW = query(i, idxV, idxW);
                // If X < V then sVW = W + X; else if V < X < W then sVW = V + W (const)
                if (sVW > V + W) {
                    // X > W (contradiction), but handle
                    A[i] = sVW - V;
                } else if (sVW < V + W) {
                    A[i] = sVW - W;
                } else {
                    // X in (V,W)
                    A[i] = sUV - U; // equals U+X
                }
            } else {
                // U < X < V => need s with V,W to get X = sVW - W
                long long sVW = query(i, idxV, idxW);
                A[i] = sVW - W;
            }
        }
    }

    // Compute final hash sum
    long long res = 0;
    long long pw = 233 % MOD;
    for (int i = 1; i <= n; ++i) {
        long long val = ((A[i] % MOD) + MOD) % MOD;
        res = (res + val * pw) % MOD;
        pw = (pw * 233) % MOD;
    }
    return (int)res;
}
