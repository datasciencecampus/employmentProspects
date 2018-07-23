# Employment Prospects Data Visualisation Tool
**Please read below before using the Employment Prospects Tool**
## Introduction
This interactive tool has been designed to provide government analysts and policymakers a platform in which to explore a variety of variables designed to provide information about employment prospects within Local Authority areas. It has has been produced in collaboration with the ONS Data Science Campus, Mango Solutions and the Cabinet Office.

The data contained in this tool has been published on the ONS website at [here](https://www.ons.gov.uk/employmentandlabourmarket/peopleinwork/employmentandemployeetypes/adhocs/007606employmentcharacteristicsoflocalauthoritiesgreatbritain2015).



## Data and composite measures
* This tool allows for the the exploration of three variables relating to the resilience of current employment and five variables relating to the potential prospects of future employment within local authorities. These are illustrated in the table below. It also includes other variables that are potentially related to employment prospects that can be explored.
* Further to this, one composite employment resilience and one employment prospect measure has been produced. They are created to provide a **potentially useful indication** of '**overall**' potential employment risks. However, they are are simple measures and should not be interpreted as fact.
* The composite variables have been created by combining the different underlying resilience and prospect variables together. This is achieved by:
    * Ranking each local authority for each underlying variable and adding the three resilience values together and the five prospect variables together. This creates two composite variables.
    * These are then standardised to a maximum value of 100.

## How to use the tool
* The tool can be accessed via the ‘Employment Tool’ button of the left.
* It allows for the comparison of two different datasets simultaneously. These datasets can be selected via the drop down menus to the left.
* By default, the tool will present each measure on a map of the UK, colouring each local authority depending on its data value. These graphs can be changed by map type (geographic map or hexagonal map) and by colour coding (either a continuous scale or 5 ‘level’ colour coding scheme).
* You are able to select different local authority areas by clicking on them on the maps or searching for them in the drop-down menu on the bottom left.
* The tool allows the measures to also be presented as
    * Ordered bar charts, where the selected Local Authorities are also highlighted.
    * A scatter plot, where the selected Local Authorities remain highlighted, and there is an additional option to colour code each point by a third variable to explore different relationships.
* The data and selected local authorities can also be compared via a data table in the final tab. This table can also be exported for use in a spreadsheet.

## Table of measures

<table id="tmeas">
  <tr>
    <th>Measure type</th>
    <th>Sectors indicator</th>
    <th>Occupations indicator</th>
    <th>Employers indicator</th>
    <th>Employees indicator</th>
  </tr>
  <tr>
    <td><em>Resilience of current jobs measure</em></td>
    <td>Proportion of employment in declining sectors</td>
    <td>Proportion of employment in declining occupations</td>
    <td>Proportion of employment concentrated in the top local employer</td>
    <td/>
  </tr>
  <tr>
    <td><em>Conditions for future jobs growth measure</em></td>
    <td>Proportion of employment in growing sectors</td>
    <td>Proportion of employment in growing occupations</td>
    <td>Net rate of business creation; Proportion of businesses with high
  growth</td>
    <td>Share of the population with degree level qualification</td>
  </tr>

</table>
