import os
import math
import unittest
import pandas as pd

class DataIntegrityTestCase(unittest.TestCase):
    def setUp(self):
        tests_dir = os.path.dirname(os.path.abspath(__file__))
        self.eds_df = pd.read_csv(os.path.join(tests_dir, "../editions.csv"))
        self.bib_df = pd.read_csv(os.path.join(tests_dir, "../bibliography.csv"))
        self.aut_df = pd.read_csv(os.path.join(tests_dir, "../authors.csv"))
        all_authors_editions = self.eds_df["author_std"].str.split("\\;\\s").tolist()
        all_authors_editions = [authors for authors in all_authors_editions if type(authors) == list]
        all_authors_editions_flattened = [author for authors in all_authors_editions for author in authors]
        self.all_authors_editions_flattened_unique = list(set(all_authors_editions_flattened))
        self.all_authors_editions_flattened_unique = [authors for authors in self.all_authors_editions_flattened_unique if authors != "OTHERS"]
        self.all_authors_authors = self.aut_df.author_std.tolist()
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

    def test_author_in_authors_uniqueness(self):
        """Tests if there are duplicated values in 'author_std' column of 'authors.csv' table"""
        duplicated_authors = self.aut_df[self.aut_df.duplicated(subset="author_std")]
        self.assertEqual(duplicated_authors.shape[0], 0,
                         msg='value(s) that is(are) duplicated: %s' % duplicated_authors["author_std"].tolist())

    def test_author_in_authors_in_author_in_editions(self):
        """Tests if there are unique values of 'author_std' column of 'authors.csv' table that are not contained in
        unique values of 'author_std' columns of 'editions.csv' table"""
        not_in_editions = list(set(self.all_authors_authors) - set(self.all_authors_editions_flattened_unique))
        self.assertEqual(len(not_in_editions), 0,
                         msg='values that are missing from unique values of "author_std" column of "editions.csv" table: %s' % not_in_editions)
        # for author in self.all_authors_authors:
          #  with self.subTest(author=author):
           #     self.assertIn(author, self.all_authors_editions_flattened_unique,
            #    msg='%s value is missing from unique values of "author_std" column of "editions.csv" table')

    def test_author_in_editions_in_author_in_authors(self):
        """Tests if there are unique values of 'author_std' column of 'editions.csv' table that are not contained in
        unique values of 'author_std' columns of 'authors.csv' table"""
        not_in_authors = list(set(self.all_authors_editions_flattened_unique) - set(self.all_authors_authors))
        self.assertEqual(len(not_in_authors), 0,
                         msg='values that are missing from unique values of "author_std" column of "authors.csv" table: %s' % not_in_authors)
        # for author in self.all_authors_editions_flattened_unique:
          #  with self.subTest(author=author):
           #     self.assertIn(author, self.all_authors_authors,
            #    msg='%s value is missing from unique values of "author_std" column of "authors.csv" table')


if __name__ == '__main__':
    unittest.main()
   
