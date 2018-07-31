/**
 * Copyright (c) 2017-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

/* List of projects/orgs using your project for the users page */
const users = [
];

const siteConfig = {
  title: 'oomd' /* title for your website */,
  tagline: 'A new userspace OOM killer',
  url: 'https://facebook.github.io' /* your website url */,
  baseUrl: '/oomd/' /* base url for your project */,
  projectName: 'oomd',
  organizationName: "facebookincubator",
  // For no header links in the top nav bar -> headerLinks: [],
  headerLinks: [
    {doc: 'overview', label: 'Docs'},
    {
      href: 'https://github.com/facebookincubator/oomd', 
      label: 'GitHub'
    },
  ],
  users,
  /* path to images for header/footer */
  headerIcon: 'img/docusaurus.svg',
  footerIcon: 'img/docusaurus.svg',
  favicon: 'img/favicon.png',
  /* colors for website */
  colors: {
    primaryColor: '#1755b7',
    secondaryColor: '#205C3B',
  },
  /* custom fonts for website */
  /*fonts: {
    myFont: [
      "Times New Roman",
      "Serif"
    ],
    myOtherFont: [
      "-apple-system",
      "system-ui"
    ]
  },*/
    
  // This copyright info is used in /core/Footer.js and blog rss/atom feeds.
  copyright:
    'Copyright (c) ' +
    new Date().getFullYear() +
    ' Facebook Inc. ',

  highlight: {
    // Highlight.js theme to use for syntax highlighting in code blocks
    theme: 'default',
  },

  // Add custom scripts here that would be placed in <script> tags
  scripts: ['https://buttons.github.io/buttons.js'],

  /* On page navigation for the current documentation page */
  onPageNav: 'separate',

  /* Open Graph and Twitter card images */
  ogImage: 'img/docusaurus.png',
  twitterImage: 'img/docusaurus.png',

  // You may provide arbitrary config keys to be used as needed by your
  // template. For example, if you need your repo's URL...
  repoUrl: 'https://github.com/facebookmicrosites/cgroup2',
};

module.exports = siteConfig;
