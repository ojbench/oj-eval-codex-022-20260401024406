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

// Compute values for 5 indices using 10 triple queries robustly
// Returns mapping from index -> value; indices provided in ids (size 5)
static bool solve_base5(const array<int,5>& ids, unordered_map<int,long long>& out_val) {
    auto key = [](int a,int b,int c){
        if (a>b) swap(a,b); if (b>c) swap(b,c); if (a>b) swap(a,b);
        return make_tuple(a,b,c);
    };
    map<tuple<int,int,int>, long long> obs;
    for (int i = 0; i < 5; ++i) for (int j = i+1; j < 5; ++j) for (int k = j+1; k < 5; ++k) {
        int x = ids[i], y = ids[j], z = ids[k];
        long long s = query(x, y, z);
        obs[key(x,y,z)] = s;
    }

    // Try all permutations as order a<b<c<d<e mapped to ids[perm[0..4]]
    array<int,5> id = ids;
    array<int,5> perm = {0,1,2,3,4};
    sort(perm.begin(), perm.end());
    do {
        int ia = id[perm[0]], ib = id[perm[1]], ic = id[perm[2]], idd = id[perm[3]], ie = id[perm[4]];
        auto getS = [&](int p,int q,int r)->long long { return obs[key(p,q,r)]; };
        long long ac = getS(ia,ib,ic);
        long long ad1 = getS(ia,ib,idd);
        long long ad2 = getS(ia,ic,idd);
        if (ad1 != ad2) continue;
        long long ae1 = getS(ia,ib,ie);
        long long ae2 = getS(ia,ic,ie);
        long long ae3 = getS(ia,idd,ie);
        if (!(ae1 == ae2 && ae2 == ae3)) continue;
        long long bd = getS(ib,ic,idd);
        long long be1 = getS(ib,ic,ie);
        long long be2 = getS(ib,idd,ie);
        if (be1 != be2) continue;
        long long ce = getS(ic,idd,ie);

        long long ae = ae1, ad = ad1, be = be1;
        long long two_a = ae + ac - ce;
        long long two_c = ac + ce - ae;
        long long two_e = ae + ce - ac;
        if ((two_a|two_c|two_e) & 1LL) continue; // parity must be even
        long long a = two_a/2;
        long long c = two_c/2;
        long long e = two_e/2;
        long long d = ad - a;
        long long b = be - e;

        // Check ordering a<b<c<d<e
        if (!(a < b && b < c && c < d && d < e)) continue;

        // Verify all 10 triples match
        vector<int> pidx = {ia,ib,ic,idd,ie};
        vector<long long> pval = {a,b,c,d,e};
        bool ok = true;
        for (int i = 0; i < 5 && ok; ++i) for (int j = i+1; j < 5 && ok; ++j) for (int k = j+1; k < 5; ++k) {
            long long mn = min(pval[i], min(pval[j], pval[k]));
            long long mx = max(pval[i], max(pval[j], pval[k]));
            long long expect = mn + mx;
            if (obs[key(pidx[i],pidx[j],pidx[k])] != expect) { ok = false; break; }
        }
        if (!ok) continue;

        // Assign to map
        for (int t = 0; t < 5; ++t) out_val[pidx[t]] = pval[t];
        return true;
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
    // Pick adjacent around median to reduce equal-case queries
    int idxU = refs[1].second; // 2nd smallest
    int idxV = refs[2].second; // 3rd smallest (between U and W)
    int idxW = refs[3].second; // 4th smallest
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
                // X > V and < W => s = U + X
                A[i] = sUV - U;
            } else if (sUV == sumUV) {
                // U < X < V → use V,W
                long long sVW = query(i, idxV, idxW);
                // Here, if X < V: sVW = X + W (< V+W); if V < X < W: sVW = V + W
                if (sVW < V + W) A[i] = sVW - W; else A[i] = sUV - U; // latter equals U+V -> cannot deduce X directly but not expected
            } else {
                // Should not happen as X is between U and W, so sUV cannot be < U+V
                long long sVW = query(i, idxV, idxW);
                if (sVW < V + W) A[i] = sVW - W; else A[i] = sUV - U;
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
