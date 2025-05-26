import numpy as np
from preflibtools.properties.subdomains.ordinal.singlepeaked import k_alternative_deletion, singlepeakedness
from preflibtools.instances import OrdinalInstance
import preflibtools
import sys
import os


class HiddenPrints:
    def __enter__(self):
        self._original_stdout = sys.stdout
        sys.stdout = open(os.devnull, 'w')

    def __exit__(self, exc_type, exc_val, exc_tb):
        sys.stdout.close()
        sys.stdout = self._original_stdout


def np_to_Ordinal(arr: np.ndarray):
    order = OrdinalInstance()
    order.append_order_array(arr)
    return order


def get_max_num_cands_for_single_peakedness(prof: np.ndarray):
    inst = np_to_Ordinal(prof)
    axis, remove = k_alternative_deletion(inst)
    return len(axis)


def get_max_num_voters_for_single_peakedness(prof: np.ndarray):
    if prof.shape[0] > 31:
        return -1
    else:
        inst = np_to_Ordinal(prof)
        with HiddenPrints():
            num_removed, _, axis, _ = singlepeakedness.approx_SP_voter_deletion_ILP(
                inst)
        return int(len(prof)-num_removed)


if __name__ == "__main__":
    get_max_num_cands_for_single_peakedness(
        np.array([
            [1, 2, 3],
            [3, 1, 2],
            [2, 3, 1]
        ])
    )
    get_max_num_voters_for_single_peakedness(
        np.array([
            [1, 2, 3],
            [3, 1, 2],
            [2, 3, 1],
        ])
    )
