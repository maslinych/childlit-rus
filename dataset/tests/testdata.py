import os
import math
import unittest
import pandas as pd

class DataIntegrityTestCase(unittest.TestCase):
    def setUp(self):
        tests_dir = os.path.dirname(os.path.abspath(__file__))
        self.eds_df = pd.read_csv(os.path.join(tests_dir, "../editions.csv"))
        self.bib_df = pd.read_csv(os.path.join(tests_dir, "../bibliography.csv"))
        self.minyear = 1917
        self.maxyear = 1985

    def test_year_inside_period(self):
        """check that year lies inside the declared data period"""
        for i, row in self.eds_df.iterrows():
            uid = '/'.join([row['vol'], row['num']])
            year = row['year']
            if not math.isnan(year):
                with self.subTest(uid=uid):
                    self.assertTrue(int(year) >= self.minyear and
                                    int(year) <= self.maxyear,
                                    msg='year outside of period %s: %s' % (uid, int(year)))

    def test_unique_vol_num_id(self):
        """test that each vol+num combination is unique"""
        uids = self.eds_df[['vol', 'num']].agg('/'.join, axis=1)
        dups = uids[uids.duplicated()]
        self.assertEqual(0, len(dups), msg='duplicate ids: %s' % ' '.join(dups))

    def test_vol_ids_present_in_bibliography(self):
        """check that vol ids in editions match those in bibliography"""
        bib_vols = list(self.bib_df['vol'])
        eds_vols = list(self.eds_df['vol'].unique())
        for vol_id in eds_vols:
            with self.subTest(vol_id=vol_id):
                self.assertTrue(vol_id in bib_vols, msg='volume id not present in bibliography: %s' % vol_id)


if __name__ == '__main__':
    unittest.main()
   
