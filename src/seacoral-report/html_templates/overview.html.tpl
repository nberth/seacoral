<!--
    Ids to replace:
    * TESTS_GENERATED (once)
    * NUM_LABELS (once)
    * COVERED_LABELS (once)
    * COVERED_PERCENTAGE (once)
    * UNCOVERABLE_LABELS (once)
    * UNCOVERABLE_PERCENTAGE (once)
    * UNKNOWN_LABELS (once)
    * UNKNOWN_PERCENTAGE (once)
    * NUM_RUNS (once)
    * OVERVIEW_RUN_TABS (once, check tab_list.html.tpl)
-->
<h2>Overview</h2>
<div id='overview-panel'> <!-- Overview panel content -->
  <div class='overview-statistics overview-subpanel'>
    <!-- Statistics panel, summing up the whole results -->
    <h3> Statistics </h3>
    <ul>
      <li>Tests generated: __TESTS_GENERATED__</li>
      <li>Labels: __NUM_LABELS__ labels to cover
	<ul>
	  <li> __COVERED_LABELS__ covered (__COVERED_PERCENTAGE__) </li>
	  <li> __UNCOVERABLE_LABELS__ uncoverable (__UNCOVERABLE_PERCENTAGE__) </li>
	  <li> __UNKNOWN_LABELS__ unknown (__UNKNOWN_PERCENTAGE__) </li>
	</ul>
      </li>
      <li> Runs: __NUM_RUNS__ </li>
    </ul>
  </div> <!-- Statistics -->
  <div class='overview-config overview-subpanel'>
    <!-- Configuration panel, with the different configurations used for each run -->
    <h3> Run details </h3>
    __OVERVIEW_RUN_TABS__
  </div> <!-- Overview panel content -->
</div> <!-- Panel associated to Overview tab -->
